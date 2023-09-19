setwd("~/Library/CloudStorage/OneDrive-Privat/Final modell 2.0")
library(tidyverse)
library(brms)
library(cmdstanr)
library(readr)

# inläsning data
f1_dat_finished  <- read_rds("dat/f1_dat_finished.rds")


#####  Antal som Random effect #####
fit_basic_antal.random_15 <- brm(
  formula = proportional_trans ~ 0 + (1 | antal) + (1 | driver) + (1 | driver:year) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_basic_antal.random_15)
write_rds(fit_basic_antal.random_15, "fit/fit_basic_antal.random_15.rds")

#### Antal som fixed effect ##### 

fit_basic_antal.fixed_15 <- brm(
  formula = prop_trans_15~ 0 +  antal + (1 | driver) + (1 | driver:year)  + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_basic_antal.fixed)
write_rds(fit_basic_antal.fixed_15, "fit/fit_basic_antal.fixed_15.rds")

####### fit  antal väder #####
fit_vader_antal.random_15 <- brm(
  formula = prop_trans_15~ 0 + (1 | antal) + (1 + weather_type | driver) + (1 | driver:year) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_vader_antal.random_15)
write_rds(fit_vader_antal.random_15, "fit/fit_vader_antal.random_15.rds")


##### fit antal bantyp ######
fit_bantyp_antal.rondom_15 <- brm(
  formula = prop_trans_15~ 0 + (1 | antal) + (1  | driver) + (1 | driver:year) + (1 + circuit_type | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_bantyp_antal.rondom_15)
write_rds(fit_bantyp_antal.rondom_15, "fit/fit_bantyp_antal.random_15.rds")















###### fit basic antal 13 ######
fit_basic_antal.random_13 <- brm(
  formula = prop_trans_13~ 0 + (1 | antal) + (1 | driver) + (1 | driver:year) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_basic_antal.random_13)
write_rds(fit_basic_antal.random_13, "fit/fit_basic_antal.random_13.rds")


##### fita vader antal 13 ######
fit_vader_antal.random_13 <- brm(
  formula = prop_trans_13~ 0 + (1 | antal) + (1 + weather_type | driver) + (1 | driver:year) + (1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_vader_antal.random_13)
write_rds(fit_vader_antal.random_13, "fit/fit_vader_antal.random_13.rds")


##### fit antal bantyp ######
fit_bantyp_antal.rondom_13 <- brm(
  formula = prop_trans_13~ 0 + (1 | antal) + (1  | driver) + (1 | driver:year) + (1 + circuit_type | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_bantyp_antal.rondom_13)
write_rds(fit_bantyp_antal.rondom_13, "fit/fit_bantyp_antal.random_13.rds")

##### fit antal vader och bantyp ######
fit_vader_bantyp_antal.rondom_13 <- brm(
  formula = proportional_trans ~ 0 + (1 | antal) + (1 + weather_type | driver) + (1 | driver:year) + (1 + circuit_type | constructor) +( 1 | constructor:year),
  family  = Beta(),
  data    = f1_dat_finished,
  backend = "cmdstanr",
  chains  = 4,
  iter    = 3500
)
summary(fit_vader_bantyp_antal.rondom_13)
write_rds(fit_vader_bantyp_antal.rondom_13, "fit/fit_vader_bantyp_antal.random_13.rds")



