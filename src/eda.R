# Dependancies -----------

source('const/glob.R')
source('src/functions.R')

df_1 <- data.table::fread('output/df_1.csv')
df_2 <- data.table::fread('output/df_2.csv')
df_a <- data.table::fread('output/df_a.csv')
df_c <- data.table::fread('output/df_c.csv')

# Raw inputs ----------------------------------------------------------

sim_time <- (10*12)

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
  dplyr::mutate(tot=breach+not_breach) %>%
  dplyr::ungroup()

capacity_0_5 <- CreateCapacity(x=60,
                           specialties=specs,
                           growth=1.049^(1/12),
                           base_capacity=base_capacity)

midterm_capacity <- capacity_0_5%>%filter(t==max(t)) %>% select(!t)

capacity_5_10 <- CreateReferrals(min_x=1,
                                 max_x=sim_time,
                                 specialty = specs,
                                 growth = referral_growth,
                                 starting_value = starting_average,
                                 jitter_factor = 0) %>% 
  filter(i <= -60) %>%
  select(!i) %>%
  rename('cap'=open) %>%
  mutate(cap = cap*0.974)
                               
capacity_ideal <- rbind(capacity_0_5,
                  capacity_5_10)

capacity_baseline <- CreateCapacity(x=sim_time,
                                    specialties=specs,
                                    growth=capacity_growth,
                                    base_capacity=base_capacity)

# Waitlist Size ----------------------------------------------------------

baseline <- CreateData(df_data = df_2,
                       ref_growth = referral_growth,
                       capacity = capacity_baseline,
                       policy = 0.5,
                       breach_limit = 4,
                       jitter_factor = 0,
                       a_lim = 0.75)$breaches %>%
  mutate(ratio = breach/tot) %>%
  add_row(old_data %>% filter(t == 0))

ideal <- CreateData(df_data = df_2,
                       ref_growth = referral_growth,
                       capacity = capacity_ideal,
                       policy = 0.5,
                       breach_limit = 4,
                       jitter_factor = 0,
                       a_lim = 0.75)$breaches %>%
  mutate(ratio = breach/tot) %>%
  add_row(old_data %>% filter(t == 0))

waitlist_plot <- ggplot() +
  geom_line(data=old_data,aes(x=t,y=tot/1e6),linetype=1,linewidth=1)+
  geom_line(data=baseline,aes(x=t,y=tot/1e6),linetype=1,col=thf,linewidth=1)+
  geom_line(data=ideal,aes(x=t,y=tot/1e6),linetype=1,col=thf2,linewidth=1)+
  geom_vline(xintercept = 0,col='gray')+
  geom_vline(xintercept = 59,col='gray')+
  theme_bw(base_size=16) +
  xlab('Months from present') +
  ylab('Number of open RTT pathways (mn)') +
  ylim(0,8)

ggsave(filename='output/waitlist_plot.png',waitlist_plot)

# Waitlist Activity & Costs -------

activity_baseline <- CreateData(df_data = df_2,
                       ref_growth = referral_growth,
                       capacity = capacity_baseline,
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
  mutate(date = max(lubridate::as_date(df_1$date)) + months(t)) 

activity_ideal <- CreateData(df_data = df_2,
                                ref_growth = referral_growth,
                             capacity = capacity_ideal,
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
  mutate(date = max(lubridate::as_date(df_1$date)) + months(t))

activity_ideal1 <- activity_ideal %>%
  pivot_longer(cols=!c('t','s','date'),names_to='activity_type',values_to='ideal_value') %>%
  mutate(measure_type = 'activity')

activity_baseline1 <- activity_baseline %>%
  pivot_longer(cols=!c('t','s','date'),names_to='activity_type',values_to='baseline_value') %>%
  mutate(measure_type = 'activity')

activity_total <- left_join(activity_ideal1,activity_baseline1,by=c('s','t','date','measure_type','activity_type'))

cost_ideal <- activity_ideal %>%
  mutate(ncl_costing = ncl * ncl_cost,
         cl_costing = cl * cl_cost,
         in_cost = ord * in_cost,
         dc_cost = dc * dc_cost,
         proc_cost = proc * proc_cost) %>%
  select(date,ncl_costing,cl_costing,in_cost,dc_cost,proc_cost) %>%
  pivot_longer(cols=!date,names_to='type',values_to='cost') %>%
  group_by(year(date),type)%>%
  summarise(cost=sum(cost,na.rm=T)/1e9)

cost_baseline <- activity_baseline %>%
  mutate(ncl_costing = ncl * ncl_cost,
         cl_costing = cl * cl_cost,
         in_cost = ord * in_cost,
         dc_cost = dc * dc_cost,
         proc_cost = proc * proc_cost) %>%
  select(date,ncl_costing,cl_costing,in_cost,dc_cost,proc_cost) %>%
  pivot_longer(cols=!date,names_to='type',values_to='cost') %>%
  group_by(year(date),type)%>%
  summarise(cost=sum(cost,na.rm=T)/1e9)

final_cost_data <- cost_baseline %>%
  rename(year='year(date)',
         base_cost = 'cost') %>%
  left_join(.,cost_ideal %>%
              rename(year='year(date)',
                     ideal_cost = 'cost'),
            by=c('year','type')) %>%
  filter(year != 2036) %>%
  group_by(type) %>%
  mutate(diff = (ideal_cost - base_cost),
         prod = 1.0058,
         pay = 1.008,
         drug = 1.025,
         val_prod = cumprod(prod),
         val_pay = cumprod(pay),
         val_drug = cumprod(drug)) %>%
  ungroup()%>%
  rowwise() %>%
  mutate(index = CreateIndex(w=w,
                               d = d,
                               r = r,
                               prod = val_prod,
                               drug = val_drug,
                               pay = val_pay,
                               deflator = 1
                               ),
         diff_fin = diff * index)

final_cost_data_total <- final_cost_data %>%
  group_by(year) %>%
  summarise(diff = sum(diff))

cost_plot <- ggplot() +
  geom_col(data= final_cost_data,aes(x=year-2024,y=diff,fill=type)) +
  geom_text(data=final_cost_data_total,aes(x=year-2024,y=diff,label=paste0('£',round(diff,1),'b'),vjust=-0.5)) +
  theme_bw(base_size =12) +
  THFstyle::scale_fill_THF()+
  labs(fill='Care activity')+
  xlab('Years from present')+
  ylab('Additional real annual cost (£bn) to clearing the backlog')

ggsave(filename='output/cost_plot.png',cost_plot)
write.csv(capacity_total,'output/capacity_total.csv')
write.csv(final_cost_data,'output/final_cost_data.csv')
write.csv(capacity_cost_ideal,'output/capacity_cost_ideal.csv')
write.csv(capacity_cost_baseline,'output/capacity_cost_baseline.csv')
