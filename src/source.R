# Source functions -------------------------------------------------------------

source('src/functions.R')
source('const/glob.R')

# Raw RTT data -------------------------------------------------------------

#Urls for datasets (1:3 means it only gets the past 3 years)
rtt_urls <- Rpublic::extract_links(rtt_link,'statistical-work-areas/rtt-waiting-times/rtt-data-')[c(1:3)]

rtt_files <- purrr::map(
  .x = rtt_urls,
  .f = Rpublic::extract_links,
  pattern = 'Full-CSV-data'
) |> 
  unlist() |> 
  unique()

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

# Wrangling ----------------------------------------------------------
 
# Wrangle data together
df_1 <- df_0 %>%
  dplyr::filter(treatment_function_code %in% specs) %>%
  # Group smaller specialties together
  dplyr::mutate(
    treatment_function_code = case_when(
      !treatment_function_code %in% glob_specs ~ 'ZZZ',
      TRUE ~ treatment_function_code
    )
  ) %>%
  dplyr::group_by(date,rtt_part_description,treatment_function_code)%>%
  dplyr::summarise_all(~sum(.x,na.rm=T)) %>%
  # This is a filter that includes ALL specialties. Remove and group by for spec split
  # dplyr::filter(treatment_function_name == 'Total') %>%
  tidyr::pivot_longer(cols=c(starts_with('gt'),total_all),names_to='metric',values_to='values') %>%
  # This is very stupid, but done either way. Effectively 'total all' includes
  # everything, but crucially also only field that has the new rtt periods
  # so we just add it as -1 then turn it back.
  dplyr::ungroup() %>%
  mutate(values = case_when(
    metric == 'total_all' & rtt_part_description != 'New RTT Periods - All Patients' ~ 0,
    T ~ values
  )) %>%
  dplyr::filter(metric != 'New RTT Periods - All Patients') %>%
  dplyr::mutate(
    i = case_when(
      metric == 'gt_104_weeks_sum_1' ~ 104,
      TRUE ~ as.numeric(str_split(metric,'_',simplify=T)[,4]))) %>%
  tidyr::pivot_wider(.,names_from='rtt_part_description',values_from='values')  %>%
  # Make date: this is how many months since START DATE which is the first date in the dataset
  dplyr::mutate(t=lubridate::interval(min(df_0$date),date) %/% months(1)) %>%
  janitor::clean_names() %>%
  #filter(metric != 'total_all') %>%
  # Sum up completed and incomplete pathways
  dplyr::mutate(
    completed = completed_pathways_for_admitted_patients + completed_pathways_for_non_admitted_patients,
    open = incomplete_pathways,
    new = new_rtt_periods_all_patients,
    #convert week to month
    i = i%/%4) %>%
  dplyr::rename('s'=treatment_function_code) %>%
  dplyr::group_by(i,t,s,date) %>%
  # Create the two main fields we need
  dplyr::summarise(
    open = sum(open,na.rm=T),
    completed = sum(completed,na.rm=T),
    new = sum(new,na.rm=T))

# This is the lagged dataset: what the previous period data was.
df_lagged <- df_1 %>%
  dplyr::mutate(t=t+1,
                i=case_when(i>=26 ~ 26,
                            TRUE ~ i+1),
                lagged_completed = completed,
                lagged_open = open) %>%
  dplyr::select(i,t,s,date,lagged_completed,lagged_open) %>%
  dplyr::group_by(i,t,s) %>%
  dplyr::summarise(lag_open = sum(lagged_open,na.rm=T))

# This is the final dataset
df_2 <- df_1 %>%
  dplyr::left_join(.,df_lagged,by=c('i','t','s')) %>%
  dplyr::mutate(carried=lag_open-completed+new,
                a=case_when(
                  #Apply cap/collar
                  open/carried > 1 ~ 1,
                  open/carried < 0 ~ 1,
                  TRUE ~ open/carried)) %>%
  dplyr::select(t,i,a,s,open,completed,carried)

# Fixed drop-off rates
df_a <- df_2 %>%
  dplyr::group_by(i,s) %>%
  dplyr::summarise(a = quantile(a,0.5,na.rm=T)) %>%
  #this is the mother of all evil: 
  #the drop-off is intensely consequential.
  rbind(data.frame('i'=27,'a'=0.97,'s'=unique(df_2$s))) %>%
  dplyr::mutate(a = case_when(is.nan(a)==TRUE ~ 0.5,
                              T ~ a),
                i=i-1) %>%
  dplyr::filter(i>=0)

# Fixed theta by year
df_c <- df_2 %>%
  dplyr::mutate(c_a = completed/(open+completed))%>%
  dplyr::group_by(i,s)
