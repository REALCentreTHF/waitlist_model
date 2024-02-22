#packages
library(tidyverse)
library (data.table)
library(xml2)
library(rvest)
library(janitor)
library(lubridate)
library(purrr)
library(performance)
library(gganimate)
library(gifski)

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

# Wrangle data together
FINAL_data <- FINAL_rtt %>%
  # This is a filter that includes ALL specialties. Remove and group by for spec split
  filter(treatment_function_name == 'Total') %>%
  pivot_longer(cols=c(starts_with('gt'),total_all),names_to='metric',values_to='values') %>%
  select(!c(treatment_function_code,treatment_function_name)) %>%
  group_by(date,rtt_part_description,metric) %>%
  # Sum up ignoring trust / spec code. Future improvement should include both
  summarise(values = sum(values,na.rm=T)) %>%
  # This is very stupid, but done either way. Effectively 'total all' includes
  # everything, but crucially also only field that has the new rtt periods
  # so we just add it as -1 then turn it back.
  mutate(
    i = case_when(
      metric=='total_all' ~ -1 ,
      metric == 'gt_104_weeks_sum_1' ~ 105,
      TRUE ~ as.numeric(str_split(metric,'_',simplify=T)[,4]))) %>%
  pivot_wider(.,names_from='rtt_part_description',values_from='values')  %>%
  # Make date
  mutate(t=interval(min(FINAL_rtt$date),date)%/% months(1)) %>%
  janitor::clean_names() %>%
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
  # Create the three main fields we need
  summarise(
    new= sum(new,na.rm=T),
    open = sum(open,na.rm=T),
    completed = sum(completed,na.rm=T)
  ) %>%
  # Added later - delete as needed.
  mutate(open = open+new) %>%
  select(!new)

# This is the lagged dataset: what the previous period data was.
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

# This is the final dataset
final <- FINAL_data %>%
  left_join(.,results_lagged,by=c('i','t')) %>%
  mutate(carried=lag_open-completed,
         a=case_when(
           open/carried > 1 ~ 0.999,
           open/carried < 0.8 ~ 0.8,
           TRUE ~ open/carried)) %>%
  select(t,i,a,open,completed,lag_open,carried)

# Variables -----------

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
stream <- data.frame('i'=-148:-1,'z'=(sample(900:1200,148)/1000)*n) %>%
  mutate(z = case_when(
    i == -3 ~ n*4,
    TRUE ~ z
  ))

df_z <- final %>%
  group_by(i) %>%
  filter(t==max(t)) %>%
  summarise(z=open) %>%
  rbind(stream)

# Capacity is defined as median 
cap <- median(capacity$capacity)

# Formula ----------------------------------------------------------

# Just to test: for now a is set to 1: i.e: no dropoffs
df_a['a'] <- 1

result <- df_z

WaitList <- function(x,cap_el,result){
  
  for(j in 0:x){
    
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
      (result['z'][result$i>=0,]*df_c['c']*cap_el)/(sum(result['z'][result$i>=0,]*df_c['c']))
    
    if(j == x){result$i <- result$i}else{result$i <- result$i + 1}
  }
  return(
    list(
      #return list of elements
      l = data.frame('i'=0:26,'z'=result['z'][result$i>=0,],t=x)
      
    ))
}

WaitTimes <- function(x,cap_el,result){
  
  for(j in 0:x){
    
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
      (result['z'][result$i>=0,]*df_c['c']*cap_el)/(sum(result['z'][result$i>=0,]*df_c['c']))
    
    result$i <- result$i + 1
  }
  return(
    list(
      #number of breaches
      b = sum(result['z'][result$i>=4,]),
      #number of waiters
      w = sum(result['z'][result$i>=0,]),
      #average wait times (w)
      r = (sum(result['z'][result$i<=25,]*c(0:26))*(4.33))/sum(result['z'][result$i<=25,])
    ))
}
# Output ----------------------------------------------------------

#test
WaitList(4,cap_el=cap,result=result)
WaitTimes(10,cap_el=cap,result=result)

#period we want to look at
t <- c(0:24)

#output for range
FINAL_outcomes <-lapply(t,
                        WaitTimes,
                        cap_el=cap,
                        result=result) %>%
  rbindlist() %>%
  tibble::rownames_to_column('t')

#Waitlist over time
data<-sapply(t,
             WaitList,
             cap_el=cap,
             result=result) %>%
  rbindlist() %>%
  mutate(flag = case_when(
    t >= 3 & i == t-3 ~ 'Y',
    TRUE ~ 'N'
  ))

# Plots ----------------------------------------------------------

thf<-'#dd0031'
thf2 <- '#2a7979'

ggplot()+
  geom_line(data=FINAL_outcomes,aes(x=as.numeric(t),y=r),col=thf)+
  scale_fill_manual(values=c('Y'=thf,'N'=thf2))+
  theme_bw()+
  xlab('Months from start')+
  ylab('Average waiting times (weeks)')

ggplot()+
  geom_line(data=FINAL_outcomes,aes(x=as.numeric(t),y=b/w),col=thf)+
  scale_fill_manual(values=c('Y'=thf,'N'=thf2))+
  theme_bw()+
  xlab('Months from start')+
  ylab('Proportion of waitlist that are 18+ week breaches')

p<- ggplot()+
  geom_col(data=data,aes(x=i,y=z/1000000,fill=flag))+
  gganimate::transition_time(t) +
  geom_vline(xintercept=3.5,linetype=2,lwd=1)+
  theme_bw()+
  scale_fill_manual(values=c('Y'=thf,'N'=thf2))+
  xlab('Months waiting') +
  ylab('Total waiting (m)')+
  theme(legend.position='none')+
  labs(title = "Month: {frame_time}")

animate(p, renderer=gifski_renderer(loop=T))  

