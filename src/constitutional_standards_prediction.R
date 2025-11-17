#Capacity from current needs to grow 6.75% given current referral stream

# Dependancies -----------

source('const/glob.R')
source('src/functions.R')

df_1 <- data.table::fread('const/budget_nov_run/df_1.csv')
df_2 <- data.table::fread('const/budget_nov_run/df_2.csv')
df_a <- data.table::fread('const/budget_nov_run/df_a.csv')
df_c <- data.table::fread('const/budget_nov_run/df_c.csv')
long_term_data <- data.table::fread('const/long_term_completed.csv') |> 
  dplyr::mutate(t=lubridate::interval(max(date2),date2) %/% months(1))

# Raw inputs ----------------------------------------------------------

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
  mean()

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


growth_stream <- df_1 %>%
  dplyr::filter(new != 0) %>%
  dplyr::ungroup() %>%
  dplyr::select(t, new, s) %>%
  dplyr::arrange(t) %>%
  tidyr::drop_na()

old_capacity <- df_1 %>%
  dplyr::mutate(t=t-max(t)) |> 
  dplyr::ungroup() %>%
  dplyr::select(t, completed) %>%
  group_by(t) |> 
  summarise(completed = sum(completed,na.rm=T)) |> 
  dplyr::arrange(t) %>%
  tidyr::drop_na()

# mean of the past 12 months
starting_average <- (growth_stream %>%
                       filter(t <= max(t) - 12))$new %>%
  median()

base_capacity <- df_1 %>%
  dplyr::group_by(t, s) %>%
  dplyr::summarise(capacity = sum(completed)) %>%
  group_by(s) %>%
  filter(t >= max(t) - 12) %>%
  summarise(cap = median(capacity, 0.75))

df_data <- df_2

df_z <- df_data %>%
  dplyr::ungroup() %>%
  # position as at latest
  dplyr::filter(t == 35) %>%
  dplyr::mutate(t = 0) %>%
  select(t, s, open, i) %>%
  add_row(
    CreateReferrals(1, sim_time,
                    specialty = specs,
                    growth = ref_growth,
                    starting_value = starting_average,
                    jitter_factor = jitter_factor
    )
  ) %>%
  rename(z = "open")

df_c <- df_c %>%
  group_by(i,t,s) %>%
  dplyr::summarise(
    c = quantile(c_a,0.5,na.rm=T),
  )

# Find ----------------------------------------------------------

baseline_breach_data <- GetPolicyFrontier(
  cap_range = c(58:70),
  referrals = df_z
)

#this gets the appropriate growth rate
growth_rate<-(baseline_breach_data |> 
  dplyr::ungroup() |> 
  dplyr::filter(abs(breach_ratio-0.08) == min(abs(((breach_ratio-0.08))))))$capacity

# Waitlist Size ----------------------------------------------------------

capacity <- CreateCapacity(x=sim_time,
                                      specialties=specs,
                                      growth=cap_baseline_growth,
                                      base_capacity=base_capacity)


activity_split_by_breach <- CreateData(df_data = df_2,
                             ref_growth = referral_growth,
                             capacity = capacity,
                             policy = policy,
                             breach_limit = breach_month,
                             jitter_factor = jitter_factor,
                             a_lim = a_lim)$breaches


total_activity <- CreateData(df_data = df_2,
                            ref_growth = referral_growth,
                            capacity = capacity,
                            policy = policy,
                            breach_limit = breach_month,
                            jitter_factor = jitter_factor,
                            a_lim = a_lim)$capacity |> 
  dplyr::mutate(date = max(df_1$date) %m+% months((t)),
                first = cap,
                fups = costs$fup_ratio * cap,
                ncl = (ncl_ratio) * (costs$fup_ratio + 1) * (1-proc_ratio)* cap,
                cl = (1-ncl_ratio) * (costs$fup_ratio + 1)* (1-proc_ratio)*cap,
                proc = (proc_ratio) * (costs$fup_ratio + 1)* cap,
                non_proc = (1-proc_ratio) * (costs$fup_ratio + 1)* cap,
                dc = costs$admit_ratio * costs$daycase_ratio * cap,
                ord = costs$admit_ratio * costs$ordinary_ratio * cap,
                ncl_cost = ncl * ncl_cost,
                cl_cost = cl * cl_cost,
                in_cost = ord * in_cost,
                dc_cost = dc * dc_cost,
                proc_cost = proc * proc_cost)

# PLOTS ----------

plot <- ggplot() +
  geom_line(data=long_term_data |> 
              mutate(cap = total,
                     s = 'C_999') |> 
              select(t,s,cap) |> 
              dplyr::rowwise() |> 
              mutate(date = max(df_1$date) %m+% months((t)))
            ,aes(x=date,y=cap/1e6),linetype=1) + 
  geom_line(data=total_activity,aes(x=date,y=cap/1e6),linetype=1)+
  theme_bw() +
  xlab('Months from present') +
  ylab('Completed RTT pathways (millions)') +
  ggtitle(
    label = 'RTT Completed Pathways',
    subtitle = 'Historic and Projected Completed pathways needed to achieve Constitutional Standards'
  )

ggsave(filename = 'const/commentary_plot.png',plot=plot)
write.csv(total_activity,'const/activity_data.csv')  

