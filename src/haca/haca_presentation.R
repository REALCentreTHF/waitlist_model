source('src/eda.R')

old_data <- old_data %>%
  mutate(ratio = breach/tot)

#Private health coverage ------

alim_high <- 0.6
alim_low <- 0.5

low_drop <- CreateData(df_data = df_2,
                       ref_growth = referral_growth,
                       capacity = capacity_baseline,
                       policy = 0.5,
                       breach_limit = 4,
                       jitter_factor = 0,
                       a_lim = alim_low)$breaches %>%
  mutate(ratio = breach/tot) %>%
  add_row(old_data %>% filter(t == 0))

high_drop <- CreateData(df_data = df_2,
                       ref_growth = referral_growth,
                       capacity = capacity_baseline,
                       policy = 0.5,
                       breach_limit = 4,
                       jitter_factor = 0,
                       a_lim = alim_high)$breaches %>%
  mutate(ratio = breach/tot) %>%
  add_row(old_data %>% filter(t == 0))

health_coverage_plot <- ggplot() +
  geom_line(data=high_drop,aes(x=t,y=tot/1e6),linetype=1,col=thf,linewidth=1.5)+
  geom_line(data=low_drop,aes(x=t,y=tot/1e6),linetype=1,col=thf2,linewidth=1.5)+
  geom_line(data=old_data,aes(x=t,y=tot/1e6),linetype=1,linewidth=1.5)+
  geom_vline(xintercept = 0,col='darkgray',linewidth=1)+
  theme_bw(base_size=16) +
  ylab('Waitlist size (mn)') +
  xlab('') +
  ylim(5,8)

health_coverage_ratio_plot <- ggplot() +
  geom_line(data=high_drop,aes(x=t,y=ratio),linetype=1,col=thf,linewidth=1.5)+
  geom_line(data=low_drop,aes(x=t,y=ratio),linetype=1,col=thf2,linewidth=1.5)+
  geom_line(data=old_data,aes(x=t,y=ratio),linetype=1,linewidth=1.5)+
  geom_vline(xintercept = 0,col='darkgray',linewidth=1)+
  theme_bw(base_size=16) +
  ylab('Breaches (%)') +
  xlab('') +
  scale_y_continuous(labels=scales::percent,limits=c(0.15,0.45)) 

#Policy divergence plot ------

policy_low <- 0.95
policy_high <- 0.05

low_treatment <- CreateData(df_data = df_2,
                       ref_growth = referral_growth,
                       capacity = capacity_baseline,
                       policy = policy_low,
                       breach_limit = 4,
                       jitter_factor = 0,
                       a_lim = 0.75)$breaches %>%
  mutate(ratio = breach/tot) %>%
  add_row(old_data %>% filter(t == 0))

high_treatment <- CreateData(df_data = df_2,
                        ref_growth = referral_growth,
                        capacity = capacity_baseline,
                        policy = policy_high,
                        breach_limit = 4,
                        jitter_factor = 0,
                        a_lim = 0.75)$breaches %>%
  mutate(ratio = breach/tot) %>%
  add_row(old_data %>% filter(t == 0))

treatment_plot <-
  ggplot() +
  geom_line(data=high_treatment,aes(x=t,y=tot/1e6),linetype=1,col=thf,linewidth=1.5)+
  geom_line(data=low_treatment,aes(x=t,y=tot/1e6),linetype=1,col=thf2,linewidth=1.5)+
  geom_line(data=old_data,aes(x=t,y=tot/1e6),linetype=1,linewidth=1.5)+
  geom_vline(xintercept = 0,col='darkgray',linewidth=1)+
  theme_bw(base_size=16) +
  ylab('Waitlist size (mn)') +
  xlab('') +
  ylim(5,8)

treatment_ratio_plot <- ggplot() +
  geom_line(data=high_treatment,aes(x=t,y=ratio),linetype=1,col=thf,linewidth=1.5)+
  geom_line(data=low_treatment,aes(x=t,y=ratio),linetype=1,col=thf2,linewidth=1.5)+
  geom_line(data=old_data,aes(x=t,y=ratio),linetype=1,linewidth=1.5)+
  geom_vline(xintercept = 0,col='darkgray',linewidth=1)+
  theme_bw(base_size=16) +
  ylab('Breaches (%)') +
  xlab('') +
  scale_y_continuous(labels=scales::percent,limits=c(0.15,0.45)) 

# Final plot ----

title_a <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "Scenario A: Waitlist under high and low provision of private healthcare",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

title_b <- cowplot::ggdraw() + 
  cowplot::draw_label(
    "Scenario B: Waitlist under high and low prioritisation of long-waiting patients",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

col_a <- cowplot::plot_grid(health_coverage_plot,health_coverage_ratio_plot,nrow=1)
col_b <- cowplot::plot_grid(treatment_plot,treatment_ratio_plot,nrow=1)

final_plot <- cowplot::plot_grid(
  title_a, 
  col_a,
  title_b,
  col_b,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

ggsave(filename='output/scenario_plot.png',final_plot)
