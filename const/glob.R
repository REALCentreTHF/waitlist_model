
glob_specs <- c('C_999',
                'C_110',
                'C_130',
                'X02',
                'C_502',
                'C_120',
                'C_100',
                'X05',
                'C_101',
                'C_301',
                'C_330',
                'X04',
                'C_320',
                'C_140')

specs <- 'C_999'

spec_names <- data.table::fread('const/spec_names.csv')

rtt_link <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/'

thf<-'#dd0031'

thf2 <- '#2a7979'

capacity_growth <- (1.025)^(1/12)
referral_growth <- (1.02)^(1/12)

costs <- data.frame(
  'metric' = c('admit_ratio','daycase_ratio','fup_ratio','ordinary_ratio'),
  'lq' = c(0.4201232,0.3414877,2.2372,0.0750734),
  'median' = c(0.42674592,0.34991,2.24522905,0.07829733),
  'uq' = c(0.4358294,0.3590254,2.2686739,0.0794380)) %>% 
  select(metric,median) %>% 
  pivot_wider(names_from='metric',values_from='median')
