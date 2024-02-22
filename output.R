#packages
library(tidyverse)
library (data.table)
library(xml2)
library(rvest)
library(janitor)
library(lubridate)
library(purrr)
library(lme4)
library(lmerTest)
library(lmtest)
library(performance)
library(see)
library(gganimate)

# Functions -------

#Gets links from any single url; string matches
get_links <- function(url_name,string){
  files <- c()
  #this is inefficient and bad practice but it's a small vector.
  for(i in seq_along(url_name)){
    pg <- rvest::read_html(url_name[i])
    pg<-(rvest::html_attr(rvest::html_nodes(pg, "a"), "href"))
    files <- c(files,pg[grepl(string,pg)])
    files <- files %>% unique()
  }
  return(files)
}

#Read all csvs from urls; unz for zips
csv_from_zip <- function(files){
  #creates temp file to read in the data
  temp <- tempfile()
  download.file(files,temp)
  #This is needed because a zip file may have multiple files
  file_names <- unzip(temp,list=T)$Name
  data<- lapply(file_names,
                function(x){
                  da <- data.table::fread(unzip(temp,x))
                  #janitor to clean unruly names
                  names(da) <- names(da) %>% janitor::make_clean_names()  
                  return(da)
                })
  #unlink the temp file, important to do
  unlink(temp)
  data}

# Raw RTT data -------------------------------------------------------------

#Urls for datasets
rtt_link <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/'
rtt_urls <- get_links(rtt_link,'statistical-work-areas/rtt-waiting-times/rtt-data-')[1:2]
rtt_files <- get_links(rtt_urls,'Full-CSV-data')

rtt_data <- sapply(rtt_files,
                   function(x){
                     csv_from_zip(x)
                   })

#Cleans the underlying data to get what we want
FINAL_rtt <- lapply(rtt_data,
                     function(x){
                       x <- x %>%
                         dplyr::mutate(date = as.Date(paste0('01-',substr(period,5,99)),'%d-%B-%Y')) %>%
                         #Select ALL specialties
                         #dplyr::filter(treatment_function_name == 'Total') %>%
                         dplyr::rename( 'trust_code' = provider_org_code) %>%
                         dplyr::select(trust_code,date,rtt_part_description,treatment_function_code,treatment_function_name,starts_with('gt'),total_all)
                     }) %>%
  rbindlist()

# Wrangling ----------------------------------------------------------

FINAL_data <- FINAL_rtt %>%
  filter(treatment_function_name == 'Total') %>%
  pivot_longer(cols=c(starts_with('gt'),total_all),names_to='metric',values_to='values') %>%
  select(!c(treatment_function_code,treatment_function_name)) %>%
  group_by(date,rtt_part_description,metric) %>%
  summarise(values = sum(values,na.rm=T)) %>%
  mutate(
    i = case_when(
      metric=='total_all' ~ -1 ,
      metric == 'gt_104_weeks_sum_1' ~ 105,
      TRUE ~ as.numeric(str_split(metric,'_',simplify=T)[,4]))) %>%
  pivot_wider(.,names_from='rtt_part_description',values_from='values')  %>%
  mutate(t=interval(min(FINAL_rtt$date),date)%/% months(1)) %>%
  clean_names() %>%
  mutate(
    completed = case_when(
      i==-1 ~ 0L,
      TRUE ~ completed_pathways_for_admitted_patients + completed_pathways_for_non_admitted_patients),
    open = case_when(
      i==-1 ~ 0L,
      TRUE ~ incomplete_pathways + incomplete_pathways_with_dta),
      new = new_rtt_periods_all_patients,
    i = case_when(i==-1 ~ 0,
                  TRUE ~ i%/%4)) %>%
  group_by(i,t) %>%
  summarise(
    new= sum(new,na.rm=T),
    open = sum(open,na.rm=T),
    completed = sum(completed,na.rm=T)
  ) %>%
  mutate(open = open+new) %>%
  select(!new)

results_lagged <- FINAL_data %>%
  mutate(t=t+1,
         i=case_when(i==26 ~ 26,
                     TRUE ~ i+1),
         lagged_completed = completed,
         lagged_open = open) %>%
  select(i,t,lagged_completed,lagged_open) %>%
  group_by(i,t) %>%
  summarise(lag_completed = sum(lagged_completed,na.rm=T),
            lag_open = sum(lagged_open,na.rm=T))

final <- FINAL_data %>%
  left_join(.,results_lagged,by=c('i','t')) %>%
  mutate(carried=lag_open-completed,
         a=case_when(
           open/carried > 1 ~ 0.999,
           open/carried < 0.8 ~ 0.8,
           TRUE ~ open/carried)) %>%
  select(t,i,a,open,completed,lag_open,carried)

df_a <- final %>%
  group_by(i) %>%
  summarise(a = mean(a,na.rm=T)) %>%
  mutate(a = case_when(is.nan(a)==TRUE ~ 0.7,
                       T ~ a))

capacity <- FINAL_data %>%
  group_by(t) %>%
  summarise(capacity = sum(completed))

df_c <- FINAL_data %>%
  group_by(i) %>%
  summarise(c=sum(completed,na.rm=T)/sum(open,na.rm=T))

n <- median((final %>% filter(i == 0))$open)

df_z <- final %>%
  group_by(i) %>%
  filter(t==max(t)) %>%
  summarise(z=open) %>%
  rbind(data.frame('i'=-148:-1,'z'=(sample(900:1300,148)/1000)*n))

cap <- median(capacity$capacity)

# Formula ----------------------------------------------------------

result <- df_z
WaitList <- function(x,capacity,result){

  for(j in 1:x){
    
    #cap i at 26
    result <- result %>%
      mutate(i = case_when(i > 26 ~ 26,
                           TRUE ~ i)) %>%
      group_by(i) %>%
      summarise(z = sum(z,na.rm=T))
    
    #find capping function,
    #apply formula
    result['z'][result$i>=0,] <- 
      #First term: z(i) * a(i)
      (result['z'][result$i>=0,]*df_a['a']) - 
      #second term: theta(i)*c
      (result['z'][result$i>=0,]*df_c['c']*capacity)/(sum(result['z'][result$i>=0,]*df_c['c']))

    result$i <- result$i + 1
  }
    return(
      list(
        #return list of elements
        l = data.frame('i'=0:27,'z'=result['z'][result$i>=0,],t=x)
        
    ))
}

WaitTimes <- function(x,capacity,result){
  
  for(j in 1:x){
    
    #cap i at 26
    result <- result %>%
      mutate(i = case_when(i > 26 ~ 26,
                           TRUE ~ i)) %>%
      group_by(i) %>%
      summarise(z = sum(z,na.rm=T))
    
    #find capping function,
    #apply formula
    result['z'][result$i>=0,] <- 
      #First term: z(i) * a(i)
      (result['z'][result$i>=0,]*df_a['a']) - 
      #second term: theta(i)*c
      (result['z'][result$i>=0,]*df_c['c']*capacity)/(sum(result['z'][result$i>=0,]*df_c['c']))
    
    result$i <- result$i + 1
  }
  return(
    list(
      #number of breaches
      b = sum(result['z'][result$i>=4,]),
      #number of waiters
      w = sum(sum(result['z'][result$i>=0,])),
      #average wait times (w)
      r = (sum(result['z'][result$i<=25,]*c(1:25))*(4.33))/sum(result['z'][result$i<=25,])
    ))
}
# Output ----------------------------------------------------------

#test
WaitList(10,capacity=cap,result=result)
WaitTimes(10,capacity=cap,result=result)

#period we want to look at
t <- c(0:24)

#output for range
FINAL_outcomes <-lapply(t,
       WaitTimes,
       capacity=cap,
       result=result) %>%
  rbindlist() %>%
  tibble::rownames_to_column('t')

#Waitlist over time
data<-sapply(t,
             WaitList,
             capacity=cap,
             result=result) %>%
  rbindlist()

# Plots ----------------------------------------------------------

ggplot()+
  geom_line(data=FINAL_output,aes(x=as.numeric(t),y=w),col='blue')+
  geom_line(data=FINAL_output,aes(x=as.numeric(t),y=b),col='red')


ggplot()+
  geom_line(data=FINAL_output,aes(x=as.numeric(t),y=r))

p<- ggplot()+
  geom_col(data=data,aes(x=i,y=z))

p + gganimate::transition_time(t) +
  labs(title = "Year: {frame_time}")

            
