# DEPENDANCIES -----------

source('const/glob.R')
source('src/functions.R')

df_1 <- data.table::fread('const/df_1.csv')
df_2 <- data.table::fread('const/df_2.csv')
df_a <- data.table::fread('const/df_a.csv')
df_c <- data.table::fread('const/df_c.csv')

# RAW INPUTS ----------------------------------------------------------

sim_time <- (11*12)

#Long-term year average
capacity_10yr <- 1.038^(1/12)
#No change 
capacity_flat <- 1^(1/12)
#CAP growth from -12t to -35t
capacity_base <- 1.015^(1/12)
  
# Fixed capacity by year
base_capacity <- df_1 %>%
  dplyr::group_by(t,s) %>%
  dplyr::summarise(capacity = sum(completed)) %>%
  group_by(s)%>%
  filter(t >= max(t)-12) %>%
  summarise(cap = quantile(capacity,0.5))

growth_stream <- df_1 %>%
  dplyr::filter(new != 0) %>%
  dplyr::ungroup()%>%
  dplyr::select(t,new,s) %>%
  dplyr::arrange(t) %>%
  tidyr::drop_na()

#mean of the past 12 months
starting_average <- (growth_stream %>% 
                       filter(t <= max(t)-12))$new %>% 
  mean

old_data <- df_2 %>%
  dplyr::mutate(t=t-max(t),
                z =  open)%>%
  dplyr::mutate(
      breach_flag = dplyr::case_when(
        i > 4 ~ 'breach',
        TRUE ~ 'not_breach')) %>%
  dplyr::group_by(t,breach_flag) %>%
  dplyr::summarise(z=sum(z)) %>%
  tidyr::pivot_wider(values_from=z,names_from=breach_flag) %>%
  dplyr::mutate(tot=breach+not_breach,
                ratio = breach/tot) %>%
  dplyr::ungroup()

old_data_time <- df_2 %>%
  dplyr::mutate(t=t-max(t),
                z =  open)%>%
  select(t,i,z) %>%
  drop_na() %>%
  dplyr::group_by(t) %>%
  dplyr::summarise(m = median(sum(i*z)/sum(z))*4.33)

capacity_ideal_10yr <- CreateCapacity(x=sim_time,
                            specialties=specs,
                            growth=capacity_10yr,
                            base_capacity=base_capacity)

capacity_ideal_flat <- CreateCapacity(x=sim_time,
                                      specialties=specs,
                                      growth=capacity_flat,
                                      base_capacity=base_capacity)

capacity_ideal_base <- CreateCapacity(x=sim_time,
                                      specialties=specs,
                                      growth=capacity_base,
                                      base_capacity=base_capacity)

# ACTIVITY ------

activity_10yr_ideal <- CreateData(df_data = df_2,
                             ref_growth = referral_growth,
                             capacity = capacity_ideal_10yr,
                             policy = 0.5,
                             breach_limit = 4,
                             jitter_factor = 0,
                             a_lim = 0.75)$capacity %>%
  mutate(
    first = cap,
    fups = costs$fup_ratio * cap,
    ncl = (ncl_ratio) * (costs$fup_ratio + 1) * (1-proc_ratio)* cap,
    cl = (1-ncl_ratio) * (costs$fup_ratio + 1)* (1-proc_ratio)*cap,
    proc = (proc_ratio) * (costs$fup_ratio + 1)* cap,
    non_proc = (1-proc_ratio) * (costs$fup_ratio + 1)* cap,
    dc = costs$admit_ratio * costs$daycase_ratio * cap,
    ord = costs$admit_ratio * costs$ordinary_ratio * cap) %>%
  select(!cap) %>%
  mutate(date = max(lubridate::as_date(df_1$date)) + months(t)) |> 
  dplyr::select(!c(t,s))

activity_flat_ideal <- CreateData(df_data = df_2,
                                  ref_growth = referral_growth,
                                  capacity = capacity_ideal_flat,
                                  policy = 0.5,
                                  breach_limit = 4,
                                  jitter_factor = 0,
                                  a_lim = 0.75)$capacity %>%
  mutate(
    first = cap,
    fups = costs$fup_ratio * cap,
    ncl = (ncl_ratio) * (costs$fup_ratio + 1) * (1-proc_ratio)* cap,
    cl = (1-ncl_ratio) * (costs$fup_ratio + 1)* (1-proc_ratio)*cap,
    proc = (proc_ratio) * (costs$fup_ratio + 1)* cap,
    non_proc = (1-proc_ratio) * (costs$fup_ratio + 1)* cap,
    dc = costs$admit_ratio * costs$daycase_ratio * cap,
    ord = costs$admit_ratio * costs$ordinary_ratio * cap) %>%
  select(!cap) %>%
  mutate(date = max(lubridate::as_date(df_1$date)) + months(t)) |> 
  dplyr::select(!c(t,s))

activity_base_ideal <- CreateData(df_data = df_2,
                                  ref_growth = referral_growth,
                                  capacity = capacity_ideal_base,
                                  policy = 0.5,
                                  breach_limit = 4,
                                  jitter_factor = 0,
                                  a_lim = 0.75)$capacity %>%
  mutate(
    first = cap,
    fups = costs$fup_ratio * cap,
    ncl = (ncl_ratio) * (costs$fup_ratio + 1) * (1-proc_ratio)* cap,
    cl = (1-ncl_ratio) * (costs$fup_ratio + 1)* (1-proc_ratio)*cap,
    proc = (proc_ratio) * (costs$fup_ratio + 1)* cap,
    non_proc = (1-proc_ratio) * (costs$fup_ratio + 1)* cap,
    dc = costs$admit_ratio * costs$daycase_ratio * cap,
    ord = costs$admit_ratio * costs$ordinary_ratio * cap) %>%
  select(!cap) %>%
  mutate(date = max(lubridate::as_date(df_1$date)) + months(t)) |> 
  dplyr::select(!c(t,s))

# COSTS ------

cost_10yr_ideal <- activity_10yr_ideal %>%
  mutate(ncl_costing = ncl * ncl_cost,
         cl_costing = cl * cl_cost,
         in_cost = ord * in_cost,
         dc_cost = dc * dc_cost,
         proc_cost = proc * proc_cost) %>%
  select(date,ncl_costing,cl_costing,in_cost,dc_cost,proc_cost) %>%
  pivot_longer(cols=!c(date),names_to='metric',values_to='values') |> 
  dplyr::group_by(date) |> 
  dplyr::summarise(ideal_cost = sum(values,na.rm=T))

cost_base_ideal <- activity_base_ideal %>%
  mutate(ncl_costing = ncl * ncl_cost,
         cl_costing = cl * cl_cost,
         in_cost = ord * in_cost,
         dc_cost = dc * dc_cost,
         proc_cost = proc * proc_cost) %>%
  select(date,ncl_costing,cl_costing,in_cost,dc_cost,proc_cost) %>%
  pivot_longer(cols=!c(date),names_to='metric',values_to='values') |> 
  dplyr::group_by(date) |> 
  dplyr::summarise(base_cost = sum(values,na.rm=T))

cost_flat_ideal <- activity_flat_ideal %>%
  mutate(ncl_costing = ncl * ncl_cost,
         cl_costing = cl * cl_cost,
         in_cost = ord * in_cost,
         dc_cost = dc * dc_cost,
         proc_cost = proc * proc_cost) %>%
  select(date,ncl_costing,cl_costing,in_cost,dc_cost,proc_cost) %>%
  pivot_longer(cols=!c(date),names_to='metric',values_to='values') |> 
  dplyr::group_by(date) |> 
  dplyr::summarise(flat_cost = sum(values,na.rm=T))

# FINAL DATASET ------

final_dataset <- activity_10yr_ideal |> 
  dplyr::select(date,first) |> 
  dplyr::rename(ideal_pathways = 'first') |> 
  dplyr::left_join(
    activity_base_ideal |> 
      dplyr::select(date,first) |> 
      dplyr::rename(base_pathways = 'first')) |> 
  dplyr::left_join(
    activity_flat_ideal |> 
      dplyr::select(date,first) |> 
      dplyr::rename(flat_pathways = 'first')) |> 
  dplyr::left_join(cost_10yr_ideal) |> 
  dplyr::left_join(cost_base_ideal) |> 
  dplyr::left_join(cost_flat_ideal)

write.csv(final_dataset,'output/hiba_output/final_dataset.csv')
