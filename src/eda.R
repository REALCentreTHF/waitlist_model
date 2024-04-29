# Synthetic growth rates -----------

source('const/glob.R')
source('src/functions.R')

df_1 <- read.csv('output/df_1.csv')
df_2 <- read.csv('output/df_2.csv')

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
  dplyr::mutate(t=t-34,
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
  geom_line(data=baseline,aes(x=t,y=tot/1e6),linetype=1,col=thf)+
  geom_line(data=baseline_jitter,aes(x=t,y=tot/1e6),linetype=2,col=thf)+
  geom_line(data=ideal,aes(x=t,y=tot/1e6),linetype=1,col=thf2)+
  geom_line(data=ideal_jitter,aes(x=t,y=tot/1e6),linetype=2,col=thf2)+
  geom_vline(xintercept = 0,col='gray')+
  theme_bw() +
  xlab('Months from present') +
  ylab('Number of open RTT pathways (mn)')

breach_plot <- ggplot() +
  geom_line(data=old_data,aes(x=t,y=(breach/(tot))),linetype=1)+
  geom_line(data=baseline,aes(x=t,y=(breach/(tot))),linetype=1,col=thf)+
  geom_line(data=baseline_jitter,aes(x=t,y=(breach/(tot))),linetype=2,col=thf)+
  geom_line(data=ideal,aes(x=t,y=(breach/(tot))),linetype=1,col=thf2)+
  geom_line(data=ideal_jitter,aes(x=t,y=(breach/(tot))),linetype=2,col=thf2)+
  geom_vline(xintercept = 0,col='gray')+
  theme_bw() +
  xlab('Months from present') +
  ylab('Proportion of 18w breaches') +
  scale_y_continuous(labels = scales::percent)
  
ggsave(filename='output/breach_plot.png',breach_plot)
ggsave(filename='output/waitlist_plot.png',waitlist_plot)

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
                                cap_growth = (1.032^(1/12)),
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

capacity_plot_ideal <- ggplot() +
  geom_col(data=capacity_ideal %>% pivot_longer(cols=!c(t,s,date),names_to='type',values_to='values'),
           aes(x=t,y=values/1e6,fill=type))+
  geom_vline(xintercept = 0,col='gray')+
  theme_bw() +
  THFstyle::scale_fill_THF()+
  xlab('Months from present') +
  ylab('Total activity needed to achieve 4m waitlist target (mn)')

ggsave(filename='output/capacity_plot.png',capacity_plot_ideal)
write.csv(capacity_ideal,'output/capacity_ideal.csv')
