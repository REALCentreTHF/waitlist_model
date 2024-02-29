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

# Source functions -------------------------------------------------------------

source('src/functions.R')

# Raw RTT data -------------------------------------------------------------

#Urls for datasets (1:3 means it only gets the past 3 years)
rtt_link <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/'
rtt_urls <- GetLinks(rtt_link,'statistical-work-areas/rtt-waiting-times/rtt-data-')[c(1:3)]
rtt_files <- GetLinks(rtt_urls,'Full-CSV-data')

#Apply function to all .zip links
rtt_data <- sapply(rtt_files,
                   function(x){
                     UnzipCSV(x)
                   })

#Cleans the underlying data to get what we want: note the data changes from apr 2021 onwards
#and data structure changes: suddenly it no longer sums up +52s and sums up to 104s+
df_0 <- lapply(rtt_data,
               function(x){
                 x %>%
                   #Create date column
                   dplyr::mutate(date = as.Date(paste0('01-',substr(period,5,99)),'%d-%B-%Y')) %>%
                   #Select ALL specialties
                   #dplyr::filter(treatment_function_name == 'Total') %>%
                   dplyr::rename( 'trust_code' = provider_org_code) %>%
                   dplyr::select(date,rtt_part_description,treatment_function_code,starts_with('gt'),total_all)
               }) %>%
  #Bind everything together
  data.table::rbindlist()

test <- df_0 %>%
  filter(treatment_function_code=='C_999')%>%
  select(date,rtt_part_description,starts_with('gt'),total_all) %>%
  dplyr::group_by(date,rtt_part_description)%>%
  dplyr::summarise_all(~sum(.x,na.rm=T)) %>%
  tidyr::pivot_longer(cols=c(starts_with('gt'),total_all),names_to='metric',values_to='values') %>%
  dplyr::mutate(
    i = case_when(
      metric == 'total_all' ~ 0 ,
      metric == 'gt_104_weeks_sum_1' ~ 104,
      TRUE ~ as.numeric(str_split(metric,'_',simplify=T)[,4]))) %>%
  tidyr::pivot_wider(.,names_from='rtt_part_description',values_from='values') %>%
  select(date,i,starts_with('Incomplete'),starts_with('New RTT')) %>%
  mutate(im = i %/% 4,
         open = `Incomplete Pathways` + `Incomplete Pathways with DTA`) %>%
  select(i,im,date,open,`New RTT Periods - All Patients`)
  
# Wrangling ----------------------------------------------------------
 
# Wrangle data together
df_1 <- df_0 %>%
  dplyr::group_by(date,rtt_part_description,treatment_function_code)%>%
  dplyr::summarise_all(~sum(.x,na.rm=T)) %>%
  # This is a filter that includes ALL specialties. Remove and group by for spec split
  # dplyr::filter(treatment_function_name == 'Total') %>%
  tidyr::pivot_longer(cols=c(starts_with('gt'),total_all),names_to='metric',values_to='values') %>%
  # This is very stupid, but done either way. Effectively 'total all' includes
  # everything, but crucially also only field that has the new rtt periods
  # so we just add it as -1 then turn it back.
  dplyr::ungroup() %>%
  dplyr::mutate(
    values = case_when(
      metric == 'total_all' & rtt_part_description != 'New RTT Periods - All Patients' ~ 0,
      TRUE ~ values
    )) %>%
  dplyr::mutate(
    i = case_when(
      metric == 'total_all' ~ 0 ,
      metric == 'gt_104_weeks_sum_1' ~ 104,
      TRUE ~ as.numeric(str_split(metric,'_',simplify=T)[,4]))) %>%
  tidyr::pivot_wider(.,names_from='rtt_part_description',values_from='values')  %>%
  # Make date: this is how many months since START DATE which is the first date in the dataset
  dplyr::mutate(t=interval(min(df_0$date),date) %/% months(1)) %>%
  janitor::clean_names() %>%
  # Sum up completed and incomplete pathways
  dplyr::mutate(
    completed = completed_pathways_for_admitted_patients + completed_pathways_for_non_admitted_patients,
    open = incomplete_pathways + incomplete_pathways_with_dta,
    new = new_rtt_periods_all_patients,
    i = i%/%4) %>%
  dplyr::rename('s'=treatment_function_code) %>%
  dplyr::group_by(i,t,s) %>%
  # Create the two main fields we need
  dplyr::summarise(
    already_open = sum(new,na.rm=T),
    new = sum(open,na.rm=T),
    completed = sum(completed,na.rm=T)) %>%
  dplyr::mutate(open = already_open + new)

# This is the lagged dataset: what the previous period data was.
df_lagged <- df_1 %>%
  dplyr::mutate(t=t+1,
                i=case_when(i==26 ~ 26,
                            TRUE ~ i+1),
                lagged_completed = completed,
                lagged_open = open) %>%
  dplyr::select(i,t,s,lagged_completed,lagged_open) %>%
  dplyr::group_by(i,t,s) %>%
  dplyr::summarise(lag_completed = sum(lagged_completed,na.rm=T),
                   lag_open = sum(lagged_open,na.rm=T))

# This is the final dataset
df_2 <- df_1 %>%
  dplyr::left_join(.,df_lagged,by=c('i','t','s')) %>%
  dplyr::mutate(carried=lag_open-completed,
                a=case_when(
                  #Apply cap/collar
                  open/carried > 1 ~ 1,
                  open/carried < 0 ~ 1,
                  TRUE ~ open/carried)) %>%
  dplyr::select(t,i,a,s,open,completed)

# Variables -----------

# Fixed drop-off rates
df_a <- df_2 %>%
  dplyr::group_by(i,s) %>%
  dplyr::summarise(a = quantile(a,0.85,na.rm=T)) %>%
  rbind(data.frame('i'=27,'a'=1,'s'=unique(df_2$s))) %>%
  dplyr::mutate(a = case_when(is.nan(a)==TRUE ~ 0.5,
                              T ~ a),
                i=i-1) %>%
  dplyr::filter(i>=0)

# Fixed capacity by year
capacity <- df_1 %>%
  dplyr::group_by(t,s) %>%
  dplyr::summarise(capacity = sum(completed))

# Fixed theta by year
df_c <- df_1 %>%
  dplyr::group_by(i,s) %>%
  dplyr::summarise(c=sum(completed,na.rm=T)/(sum(open,na.rm=T)+sum(completed,na.rm=T)))

#Create dataset; rbind to stream for test
df_z <- df_2 %>%
  #dplyr::group_by(i) %>%
  dplyr::mutate(open = as.numeric(open),
                i = case_when(i == 0 ~ i-t,
                              TRUE ~ i))%>%
  dplyr::filter(i + t == 0 | t == 0) %>%
  dplyr::rename('z'=open) %>%
  dplyr::select(!a)

# Capacity is defined as min 
cap <- capacity %>% group_by(s)%>%
  summarise(cap=min(capacity))

# Output ----------------------------------------------------------

# Just to test: for now a is set to 1: i.e: no dropoffs
#df_a['a'] <- 0.90

#period we want to look at
t <- c(1:32)

#output for range
outcomes <-lapply(t,
                        WaitTimes,
                        cap_el=cap,
                        result=df_z,
                        df_a=df_a,
                        df_c=df_c,
                        breach = 3) %>%
  rbindlist() %>%
  tibble::rownames_to_column('t') %>%
  mutate(ratio = b/w)

#Waitlist over time
data<-sapply(t,
             WaitList,
             cap_el=cap,
             result=df_z,
             df_a=df_a,
             df_c=df_c) %>%
  rbindlist()

# Plots ----------------------------------------------------------

thf<-'#dd0031'
thf2 <- '#2a7979'

test <- data %>% filter(s == 'C_999') %>%
  select(i,t,z)

test2 <- test %>%
  pivot_wider(names_from=t,values_from=z)

ggplot()+
  geom_line(data=test,aes(x=t,y=z,col=as.character(i)))

ggplot()+
  geom_col(data=test,aes(x=i,y=z/1000000),fill=thf)+
  gganimate::transition_time(t) +
  geom_vline(xintercept=3.5,linetype=2,lwd=1)+
  theme_bw()+
  xlab('Months waiting') +
  ylab('Total waiting (m)')+
  labs(title = "Month: {frame_time}") +
  ease_aes('linear')

animate(p, renderer=gifski_renderer(loop=T),nframes = 32)  

q<- ggplot()+
  geom_col(data=df_2 %>% filter(s=='C_999'),aes(x=i,y=open/1000000))+
  gganimate::transition_time(as.integer(t)) +
  geom_vline(xintercept=3.5,linetype=2,lwd=1)+
  theme_bw()+
  scale_fill_manual(values=c('Y'=thf,'N'=thf2))+
  xlab('Months waiting') +
  ylab('Total waiting (m)')+
  theme(legend.position='none')+
  labs(title = "Month: {frame_time}")

animate(q, renderer=gifski_renderer(loop=T))  

test <- data %>% 
  left_join(.,df_2 %>% select(i,t,s,open),by=c('i','s','t')) %>% 
  mutate(val =(z-open)) %>%
  filter(s == 'C_999') %>%
  filter(i!=0)

p<- ggplot()+
  geom_col(data=test,aes(x=i,y=val))+
  gganimate::transition_time(as.integer(t)) +
  theme_bw()+
  xlab('Months waiting') +
  ylab('% deviation from reality')+
  theme(legend.position='none')+
  labs(title = "Month: {frame_time}")

animate(p, renderer=gifski_renderer(loop=T),nframes=32)  

diagnostics <- data %>%
  mutate(type = 'Test') %>%
  select(t,i,s,z,type) %>%
  rbind(df_2 %>% 
          filter(i!=0) %>% 
          select(!c(a,completed)) %>% 
          rename('z'=open) %>% 
          mutate(type = 'Real')) %>%
  filter(t != 0) %>% 
  filter(s == 'C_100')

d<-ggplot()+
  geom_col(data=diagnostics,aes(x=i,y=z,fill=type))+
  gganimate::transition_time(as.integer(t)) +
  facet_wrap(~type)+
  theme_bw()+
  scale_fill_manual(values=c('Real'=thf2,'Test'=thf))+
  xlab('Months waiting') +
  ylab('Total waiting (m)')+
  theme(legend.position='none')+
  labs(title = "Month: {frame_time}")

animate(d, renderer=gifski_renderer(loop=T),nframes=32,fps=3)  