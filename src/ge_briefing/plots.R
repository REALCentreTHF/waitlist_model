# Dependencies ------

source('src/ge_briefing/eda.R')

# Median wait times ------

wait_times_plot <- ggplot() +
  geom_line(data=old_data_time,aes(x=t,y=m),linetype=1,linewidth=1)+
  geom_line(data=full_10yr_data,aes(x=t,y=mean),linetype=2,col=thf,linewidth=1)+
  geom_line(data=full_5yr_data,aes(x=t,y=mean),linetype=2,col=thf2,linewidth=1)+
  geom_vline(xintercept = 0,col='gray')+
  geom_vline(xintercept = 59,col='gray')+
  theme_bw(base_size=16) +
  xlab('Months from present') +
  ylab('Mean wait times (weeks)') +
  ylim(0,20)

ggsave(filename='output/wait_times_plot.png',wait_times_plot)

# Waitlist Size ------

wait_size_plot <- ggplot() +
  geom_line(data=old_data,aes(x=t,y=tot/1e6),linetype=1,linewidth=1)+
  geom_line(data=breach_10yr_data,aes(x=t,y=tot/1e6),linetype=2,col=thf,linewidth=1)+
  geom_line(data=breach_5yr_data,aes(x=t,y=tot/1e6),linetype=2,col=thf2,linewidth=1)+
  geom_vline(xintercept = 0,col='gray')+
  geom_vline(xintercept = 59,col='gray')+
  theme_bw(base_size=16) +
  xlab('Months from present') +
  ylab('Total size of RTT waitlist (mn)') +
  ylim(0,8)

ggsave(filename='output/wait_size_plot.png',wait_size_plot)

# Number of Breaches ------

breach_plot <- ggplot() +
  geom_line(data=old_data,aes(x=t,y=ratio),linetype=1,linewidth=1)+
  geom_line(data=breach_10yr_data,aes(x=t,y=ratio),linetype=2,col=thf,linewidth=1)+
  geom_line(data=breach_5yr_data,aes(x=t,y=ratio),linetype=2,col=thf2,linewidth=1)+
  geom_vline(xintercept = 0,col='gray')+
  geom_vline(xintercept = 59,col='gray')+
  theme_bw(base_size=16) +
  scale_y_continuous(labels=scales::percent,limits=c(0,0.5))+
  xlab('Months from present') +
  ylab('Proportion of 18w breaches') +
  geom_hline(yintercept=0.08)

ggsave(filename='output/breach_plot.png',breach_plot)

# Cost over time -----

cost_plot <- ggplot() +
  geom_line(data=final_cost_data,aes(x=year,y=cost_5yr),col=thf2,linewidth=1)+
  geom_line(data=final_cost_data,aes(x=year,y=cost_10yr),col=thf,linewidth=1)+
  theme_bw(base_size=16) +
  xlab('') +
  ylab('Required funding (£bn)')

ggsave(filename='output/cost_plot.png',cost_plot)

write.csv(old_data,'output/breach_old.csv')
write.csv(breach_10yr_data,'output/breach_10yr.csv')
write.csv(breach_5yr_data,'output/breach_5yr.csv')
