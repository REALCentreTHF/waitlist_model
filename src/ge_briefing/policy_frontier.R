# Dependancies -----------

source("const/glob.R")
source("src/functions.R")

df_1 <- data.table::fread("const/df_1.csv") %>%
  select(!V1)
df_2 <- data.table::fread("const/df_2.csv")%>%
  select(!V1)
df_a <- data.table::fread("const/df_a.csv")%>%
  select(!V1)
df_c <- data.table::fread("const/df_c.csv")%>%
  select(!V1)

growth_stream <- df_1 %>%
  dplyr::filter(new != 0) %>%
  dplyr::ungroup() %>%
  dplyr::select(t, new, s) %>%
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

# Create Outputs -----------

df_c <- df_c %>%
  group_by(i,t,s) %>%
  dplyr::summarise(
    c = quantile(c_a,0.5,na.rm=T),
  )

breach_ratio <- expand_grid(
  capacity = ((1000 + c(40:80)) / 1000)
) %>%
  rowwise() %>%
  mutate(breach_ratio = GetBreachRatio(referrals = df_z,c_growth = capacity^(1 / 12), breach_limit = 4))

df_z_2 <- df_z

waitlist_size <- expand_grid(
  capacity = ((1000 + c(0:60)) / 1000)
) %>%
  rowwise() %>%
  mutate(waitlist_size = GetTotalWaitlist(c_growth = capacity^(1 / 12)))

