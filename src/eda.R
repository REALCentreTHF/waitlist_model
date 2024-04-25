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

growth_average <- (1+median(growth_stream$g))^(1/12)
starting_average <- median(growth_stream$open)

df_z_2 <- df_2 %>%
  dplyr::ungroup()%>%
  #position as at latest
  dplyr::filter(t == max(t)) %>%
  #set latest to 0
  dplyr::mutate(t=0) %>%
  select(t,s,open,i) %>%
  add_row(
    CreateReferrals(1,sim_time,
                    specialty=specs,
                    growth=growth_average,
                    starting_value=starting_average)
  ) %>%
  rename(z='open')

cap <- CreateCapacity(x=sim_time,
                        specialties=specs,
                        growth=1.00083,
                        base_capacity=base_capacity)

# Output ----------------------------------------------------------

# Just to test: for now a is set to 1: i.e: no dropoffs
#df_a['a'] <- 1

#period we want to look at
t <- c(1:sim_time)

#Waitlist over time
data<-lapply(t,
             WaitList,
             cap_el=cap,
             result=df_z_2,
             df_a=df_a,
             df_c=df_c) %>%
  purrr::flatten()%>%
  data.table::rbindlist()
  # In doing this, I incidentally discovered an issue in either
  # base R or data.table...despite considering i as an integer,
  # and even AFTER coercing it to an int, it still treats it
  # as a float with a floating point error, which means it won't
  # make it function properly in gganimate...so i've had to order it
  # as a factor!
  mutate(i = factor(as.character(i),
                    levels = c('0',"1", "2", "3", "4", "5",
                               "6", "7", "8", "9", "10",
                               "11", "12", "13", "14", "15",
                               "16", "17", "18", "19", "20",
                               "21", "22", "23", "24", "25",'26')))

# checks ----------------------------------------------------------

breaches <- data %>%
    mutate(
      breach_flag = case_when(
        i > 3 ~ 'above_18w',
        TRUE ~ 'below_18w')) %>%
    ungroup()%>%
    group_by(t,breach_flag) %>%
    summarise(z = sum(z)) %>%
    pivot_wider(values_from=z,names_from=breach_flag) %>%
    mutate(ratio_breach = above_18w / (above_18w+below_18w),
           tot = above_18w+below_18w)

ggplot(data=breaches) +
  geom_line(aes(x=t,y=tot/1e6))+
  theme_bw()+
  ylim(0,8)

# Plots ----------------------------------------------------------

thf<-'#dd0031'
thf2 <- '#2a7979'

p<-ggplot()+
  geom_col(data=data,aes(x=i,y=z/1000),fill=thf)+
  gganimate::transition_time(t)+
  geom_vline(xintercept=3.5,linetype=2,lwd=1)+
  theme_bw()+
  xlab('Months waiting') +
  ylab('Total waiting (th)')+
  theme(legend.position='none')+
  labs(title = "Month: {frame_time}")

animate(p, renderer=gifski_renderer(loop=T))  

q<- ggplot()+
  geom_col(data=data.table(df_2),aes(x=i,y=open/1000000))+
  gganimate::transition_time(as.integer(t)) +
  geom_vline(xintercept=3.5,linetype=2,lwd=1)+
  theme_bw()+
  scale_fill_manual(values=c('Y'=thf,'N'=thf2))+
  xlab('Months waiting') +
  ylab('Total waiting (m)')+
  theme(legend.position='none')+
  labs(title = "Month: {frame_time}")

animate(q, renderer=gifski_renderer(loop=T))  

test <- data %>% 
  left_join(.,df_2 %>% select(i,t,s,open),by=c('i','s','t')) %>% 
  mutate(val =(z-open)) %>%
  filter(s == 'C_999') %>%
  filter(i!=0)

p<- ggplot()+
  geom_col(data=test,aes(x=i,y=val))+
  gganimate::transition_time(as.integer(t)) +
  theme_bw()+
  xlab('Months waiting') +
  ylab('% deviation from reality')+
  theme(legend.position='none')+
  labs(title = "Month: {frame_time}")

animate(p, renderer=gifski_renderer(loop=T),nframes=32)  

diagnostics <- data %>%
  mutate(type = 'Test') %>%
  select(t,i,s,z,type) %>%
  rbind(df_2 %>% 
          filter(i!=0) %>% 
          select(!c(a,completed)) %>% 
          rename('z'=open) %>% 
          mutate(type = 'Real')) %>%
  filter(t != 0) %>% 
  filter(s == 'C_100')

d<-ggplot()+
  geom_col(data=diagnostics,aes(x=i,y=z,fill=type))+
  gganimate::transition_time(as.integer(t)) +
  facet_wrap(~type)+
  theme_bw()+
  scale_fill_manual(values=c('Real'=thf2,'Test'=thf))+
  xlab('Months waiting') +
  ylab('Total waiting (m)')+
  theme(legend.position='none')+
  labs(title = "Month: {frame_time}")

animate(d, renderer=gifski_renderer(loop=T),nframes=32,fps=3)  

ggplot(data=data %>% 
         group_by(i,s) %>% 
         summarise(a = mean(a),c=mean(c)) %>%
         left_join(.,spec_names,by=c('s'='treatment_function_code')))+
  geom_line(aes(x=i,y=c,group=s,col=treatment_function_name),alpha=1) +
  theme_bw()

ggplot(data=data %>% 
         group_by(i,s) %>% 
         summarise(a = mean(a),c=mean(c)) %>%
         left_join(.,spec_names,by=c('s'='treatment_function_code')))+
  geom_line(aes(x=i,y=c,group=s,col=treatment_function_name),alpha=1) +
  theme_bw()

