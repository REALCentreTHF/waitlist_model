
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

capacity_growth <- 1.00083
referral_growth <- 1.001652
