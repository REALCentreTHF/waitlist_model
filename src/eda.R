# Synthetic growth rates -----------

sim_time <- 72

growth_stream <- df_2 %>%
  dplyr::filter(i == 0) %>%
  dplyr::ungroup()%>%
  dplyr::select(t,open,s) %>%
  dplyr::arrange(t) %>%
  group_by(s) %>%
  dplyr::mutate(
    g = (open - lag(open))/lag(open)
  ) %>%
  drop_na()

starting_average <- 1.5*1e6

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


df_a['a'] <- 0.98


baseline <- CreateData(ref_growth = referral_growth,
                       cap_growth = 1.001652,
                       policy = 0.5,
                       breach_limit = 4,
                       jitter_factor = 0)$breaches %>%
  add_row(
    old_data %>% filter(t == 0))

baseline_shocks <- CreateData(ref_growth = referral_growth,
                       cap_growth = 1.001652,
                       policy = 0.5,
                       breach_limit = 4,
                       jitter_factor = 100)$breaches %>%
  add_row(
    old_data %>% filter(t == 0))


ideal <- CreateData(ref_growth = referral_growth,
                       cap_growth = 1.002478,
                       policy = 0.5,
                       breach_limit = 4,
                       jitter_factor = 0)$breaches %>%
  add_row(
    old_data %>% filter(t == 0))

ideal_shocks <- CreateData(ref_growth = referral_growth,
                              cap_growth = 1.002478,
                              policy = 0.5,
                              breach_limit = 4,
                              jitter_factor = 100)$breaches %>%
  add_row(
    old_data %>% filter(t == 0))

### Outputs -----

ggplot() +
  geom_line(data=old_data,aes(x=t,y=tot/1e6),linetype=1)+
  geom_line(data=baseline,aes(x=t,y=tot/1e6),linetype=2,col=thf)+
  geom_line(data=baseline_shocks,aes(x=t,y=tot/1e6),linetype=1,col=thf)+
  geom_line(data=ideal,aes(x=t,y=tot/1e6),linetype=2,col=thf2)+
  geom_line(data=ideal_shocks,aes(x=t,y=tot/1e6),linetype=1,col=thf2)+
  geom_vline(xintercept = 0,col='gray')+
  theme_bw() +
  xlab('Months from present') +
  ylab('Number of open RTT pathways (mn)')


ggplot() +
  geom_line(data=old_data,aes(x=t,y=breach/1e6),linetype=1)+
  geom_line(data=baseline,aes(x=t,y=breach/1e6),linetype=2,col=thf)+
  geom_line(data=baseline_shocks,aes(x=t,y=breach/1e6),linetype=1,col=thf)+
  geom_line(data=ideal,aes(x=t,y=breach/1e6),linetype=2,col=thf2)+
  geom_line(data=ideal_shocks,aes(x=t,y=breach/1e6),linetype=1,col=thf2)+
  geom_vline(xintercept = 0,col='gray')+
  theme_bw() +
  xlab('Months from present') +
  ylab('Number of over 18-week waits (mn)')
