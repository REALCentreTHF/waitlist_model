#Capacity from current needs to grow 6.75% given current referral stream

# Dependancies -----------

source('const/glob.R')
source('src/functions.R')

df_1 <- data.table::fread('const/df_1.csv')
df_2 <- data.table::fread('const/df_2.csv')
df_a <- data.table::fread('const/df_a.csv')
df_c <- data.table::fread('const/df_c.csv')
long_term_data <- data.table::fread('const/long_term_completed.csv') |> 
  dplyr::mutate(t=lubridate::interval(max(date2),date2) %/% months(1))

# Raw inputs ----------------------------------------------------------

sim_time <- (51)
low_ref_growth <- 1.020^(1/12)
high_ref_growth <- 1.036^(1/12)

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
#50 MONTHS from now is MARCH 2029 (latest)
sim_time <- 51
ref_growth <- referral_growth
jitter_factor <- 0
a_lim <- 0.75
policy <- 0.5

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

df_z_high <- df_data %>%
  dplyr::ungroup() %>%
  # position as at latest
  dplyr::filter(t == 35) %>%
  dplyr::mutate(t = 0) %>%
  select(t, s, open, i) %>%
  add_row(
    CreateReferrals(1, sim_time,
                    specialty = specs,
                    growth = high_ref_growth,
                    starting_value = starting_average,
                    jitter_factor = jitter_factor
    )
  ) %>%
  rename(z = "open")

df_z_low <- df_data %>%
  dplyr::ungroup() %>%
  # position as at latest
  dplyr::filter(t == 35) %>%
  dplyr::mutate(t = 0) %>%
  select(t, s, open, i) %>%
  add_row(
    CreateReferrals(1, sim_time,
                    specialty = specs,
                    growth = low_ref_growth,
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

# Ranges needed ----------------------------------------------------------

baseline_breach_data <- GetPolicyFrontier(
  cap_range = c(58:70),
  referrals = df_z
)

low_breach_data <- GetPolicyFrontier(
  cap_range = c(58:70),
  referrals = df_z_low
)

high_breach_data <- GetPolicyFrontier(
  cap_range = c(70:80),
  referrals = df_z_high
)

# Waitlist Size ----------------------------------------------------------

#Expected range is between 6.78%, and 6.0% and 7.6%
cap_baseline_growth <- 1.0685 ^(1/12)
cap_low_growth <- 1.06 ^(1/12)
cap_high_growth <- 1.076 ^(1/12)

capacity <- CreateCapacity(x=sim_time,
                                      specialties=specs,
                                      growth=cap_baseline_growth,
                                      base_capacity=base_capacity)

capacity_low <- CreateCapacity(x=sim_time,
                           specialties=specs,
                           growth=cap_low_growth,
                           base_capacity=base_capacity)

capacity_high <- CreateCapacity(x=sim_time,
                           specialties=specs,
                           growth=cap_high_growth,
                           base_capacity=base_capacity)

baseline_data <- CreateData(df_data = df_2,
                             ref_growth = referral_growth,
                             capacity = capacity,
                             policy = 0.5,
                             breach_limit = 4,
                             jitter_factor = 0,
                             a_lim = 0.75)$breaches

high_data <- CreateData(df_data = df_2,
                        ref_growth = high_ref_growth,
                        capacity = capacity_high,
                        policy = 0.5,
                        breach_limit = 4,
                        jitter_factor = 0,
                        a_lim = 0.75)$breaches

low_data <- CreateData(df_data = df_2,
                        ref_growth = low_ref_growth,
                        capacity = capacity_low,
                        policy = 0.5,
                        breach_limit = 4,
                        jitter_factor = 0,
                        a_lim = 0.75)$breaches

baseline_data_activity <- CreateData(df_data = df_2,
                            ref_growth = referral_growth,
                            capacity = capacity,
                            policy = 0.5,
                            breach_limit = 4,
                            jitter_factor = 0,
                            a_lim = 0.75)$capacity |> 
  mutate(date = make_date(year = 2024, month = 9, day = 1) %m+% months((t)))

high_data_activity <- CreateData(df_data = df_2,
                        ref_growth = high_ref_growth,
                        capacity = capacity_high,
                        policy = 0.5,
                        breach_limit = 4,
                        jitter_factor = 0,
                        a_lim = 0.75)$capacity |> 
  mutate(date = make_date(year = 2024, month = 9, day = 1) %m+% months((t)))


low_data_activity <- CreateData(df_data = df_2,
                       ref_growth = low_ref_growth,
                       capacity = capacity_low,
                       policy = 0.5,
                       breach_limit = 4,
                       jitter_factor = 0,
                       a_lim = 0.75)$capacity |> 
  mutate(date = make_date(year = 2024, month = 9, day = 1) %m+% months((t)))


# PLOTS ----------

plot <- ggplot() +
  geom_line(data=long_term_data |> 
              mutate(cap = total,
                     s = 'C_999') |> 
              select(t,s,cap) |> 
              base::rbind(low_data_activity |> 
                      filter(t == 1) |> 
                        select(!date)) |> 
              dplyr::rowwise() |> 
              mutate(date = make_date(year = 2024, month = 9, day = 1) %m+% months((t)))
            ,aes(x=date,y=cap/1e6),linetype=1) + 
  geom_line(data=low_data_activity,aes(x=date,y=cap/1e6),linetype=2)+
  geom_line(data=baseline_data_activity,aes(x=date,y=cap/1e6),linetype=1)+
  geom_line(data=high_data_activity,aes(x=date,y=cap/1e6),linetype=2) +
  theme_bw() +
  xlab('Months from present') +
  ylab('Completed RTT pathways (millions)') +
  ggtitle(
    label = 'RTT Completed Pathways',
    subtitle = 'Historic and Projected Completed pathways needed to achieve Constitutional Standards'
  )

all_act_data <- baseline_data_activity |> 
  mutate(type = 'baseline') |> 
  rbind(
    low_data_activity |> 
      mutate(type = 'low_referral_growth')
  ) |> 
  rbind(high_data_activity |> 
          mutate(type = 'high_referral_growth'))

ggsave(filename = 'const/commentary_plot.png',plot=plot)
write.csv(all_act_data,'const/activity_data.csv')  

