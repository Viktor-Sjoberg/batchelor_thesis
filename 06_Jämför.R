setwd("~/Library/CloudStorage/OneDrive-Privat/Final modell 2.0")
library(readr)
library(brms)
library(xtable)
library(bayesplot)
library(ggplot2)
library(Matrix)
library(tidyverse)

fit_basic_antal_13 <- read_rds("fit/fit_basic_antal.random_13.rds") %>% add_criterion("loo")
fit_vader_antal_13 <- read_rds("fit/fit_vader_antal.random_13.rds") %>% add_criterion("loo")
fit_bantyp_antal_13 <- read_rds("fit/fit_bantyp_antal.random_13.rds") %>% add_criterion("loo")
fit_vader_bantyp_antal_13 <- read_rds("fit/fit_vader_bantyp_antal.random_13.rds") %>% add_criterion("loo")

loo_results <- loo_compare(
  fit_basic_antal_13,
  fit_vader_antal_13,
  fit_bantyp_antal_13,
  fit_vader_bantyp_antal_13,
  model_names = c("Basic", "Väder", "Bantyp", "Väder + Bantyp")
)

loo_results
write_rds(loo_results, "fit/loo_results_antal.rds")
loo_results <- read_rds("fit/loo_results_antal.rds")
xtable::xtable(loo_results)
