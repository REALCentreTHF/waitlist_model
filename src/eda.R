# Synthetic growth rates -----------

source('src/source.R')
source('const/glob.R')

### EDA

sim_time <- 72+(5*12)

growth_stream <- df_1 %>%
  dplyr::filter(new != 0) %>%
  dplyr::ungroup()%>%
  dplyr::select(t,new,s) %>%
  dplyr::arrange(t) %>%
  drop_na()

starting_average <- growth_stream$new %>% median

# checks ----------------------------------------------------------

old_data <- df_2 %>%
  mutate(t=t-34)%>%
  mutate(z =  open)%>%
  select(i,s,z,t) %>%
  mutate(
      breach_flag = case_when(
        i > 4 ~ 'breach',
        TRUE ~ 'not_breach')) %>%
    ungroup()%>%
    group_by(t,breach_flag) %>%
    summarise(z=sum(z)) %>%
    pivot_wider(values_from=z,names_from=breach_flag) %>%
    mutate(tot=breach+not_breach)%>%
  ungroup()

### calculations


baseline_jitter <- CreateData(df_data = df_2,
                              ref_growth = referral_growth,
                              cap_growth = capacity_growth,
                              policy = 0.5,
                              breach_limit = 4,
                              jitter_factor = 100,
                              a_lim = 0.75)$breaches %>%
  add_row(
    old_data %>% filter(t == 0))


baseline <- CreateData(df_data = df_2,
                       ref_growth = referral_growth,
                       cap_growth = capacity_growth,
                       policy = 0.5,
                       breach_limit = 4,
                       jitter_factor = 0,
                       a_lim = 0.75)$breaches %>%
  add_row(
    old_data %>% filter(t == 0))

### best case scenario
ideal_jitter <- CreateData(df_data = df_2,
                              ref_growth = referral_growth,
                              cap_growth = (1.032^(1/12)),
                              policy = 0.5,
                              breach_limit = 4,
                              jitter_factor = 100,
                              a_lim = 0.75)$breaches %>%
  add_row(
    old_data %>% filter(t == 0))


ideal <- CreateData(df_data = df_2,
                       ref_growth = referral_growth,
                       cap_growth = (1.032^(1/12)),
                       policy = 0.5,
                       breach_limit = 4,
                       jitter_factor = 0,
                       a_lim = 0.75)$breaches %>%
  add_row(
    old_data %>% filter(t == 0))

waitlist_plot <- ggplot() +
  geom_line(data=old_data,aes(x=t,y=tot/1e6),linetype=1)+
  geom_line(data=baseline,aes(x=t,y=tot/1e6),linetype=2,col=thf)+
  geom_line(data=baseline_jitter,aes(x=t,y=tot/1e6),linetype=1,col=thf)+
  geom_line(data=ideal,aes(x=t,y=tot/1e6),linetype=2,col=thf2)+
  geom_line(data=ideal_jitter,aes(x=t,y=tot/1e6),linetype=1,col=thf2)+
  geom_vline(xintercept = 0,col='gray')+
  theme_bw() +
  xlab('Months from present') +
  ylab('Number of open RTT pathways (mn)')

breach_plot <- ggplot() +
  geom_line(data=old_data,aes(x=t,y=(breach/(tot))),linetype=1)+
  geom_line(data=baseline,aes(x=t,y=(breach/(tot))),linetype=2,col=thf)+
  geom_line(data=baseline_jitter,aes(x=t,y=(breach/(tot))),linetype=1,col=thf)+
  geom_line(data=ideal,aes(x=t,y=(breach/(tot))),linetype=2,col=thf2)+
  geom_line(data=ideal_jitter,aes(x=t,y=(breach/(tot))),linetype=1,col=thf2)+
  geom_vline(xintercept = 0,col='gray')+
  theme_bw() +
  xlab('Months from present') +
  ylab('Proportion of 18w breaches') +
  scale_y_continuous(labels = scales::percent)
  

## costs:

capacity_baseline <- CreateData(df_data = df_2,
                       ref_growth = referral_growth,
                       cap_growth = capacity_growth,
                       policy = 0.5,
                       breach_limit = 4,
                       jitter_factor = 0,
                       a_lim = 0.75)$capacity %>%
  mutate(
    first = cap,
    fups = costs$fup_ratio * cap,
    dc = costs$admit_ratio * costs$daycase_ratio * cap,
    ord = costs$admit_ratio * costs$ordinary_ratio * cap) %>%
  select(!cap) %>%
  mutate(date = max(df_1$date) + months(t))

capacity_ideal <- CreateData(df_data = df_2,
                                ref_growth = referral_growth,
                                cap_growth = (1.03^(1/12)),
                                policy = 0.5,
                                breach_limit = 4,
                                jitter_factor = 0,
                                a_lim = 0.75)$capacity %>%
  mutate(
    first = cap,
    fups = costs$fup_ratio * cap,
    dc = costs$admit_ratio * costs$daycase_ratio * cap,
    ord = costs$admit_ratio * costs$ordinary_ratio * cap) %>%
  select(!cap) %>%
  mutate(date = max(df_1$date) + months(t))

##

goals <- expand_grid(
  'ref_growth' = c(-20:20/100),
  'cap_growth' = c(-20:20/100)
) %>%
  dplyr::rowwise()%>%
  mutate(
    goal = CreateGoals(
      df_data = df_2,
      ref_growth = ref_growth,
      cap_growth = cap_growth,
      policy = 0.5,
      breach_limit = 4,
      jitter_factor = 0,
      a_lim = 0.75
    )
  )

goals_limit <- expand_grid(
  'ref_growth' = (1+c(0,0.01,0.02,0.03,0.04,0.05))^(1/12),
  'cap_growth' = (1+c(-100:100/1000))^(1/12)
) %>%
  dplyr::rowwise()%>%
  mutate(
    goal = CreateGoals(
      df_data = df_2,
      ref_growth = ref_growth,
      cap_growth = cap_growth,
      policy = 0.5,
      breach_limit = 4,
      jitter_factor = 0,
      a_lim = 0.75
    )
  )
  
write.csv(goals,'output/goals.csv')

ggplot()+
  geom_function()
