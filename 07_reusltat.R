setwd("~/Library/CloudStorage/OneDrive-Privat/Final modell 2.0") 

library(tidyverse)
library(brms)
library(patchwork)
library(glue)
library(TTR)
library(ggplot2)
library(xtable)
library(rstanarm)
library("bayesplot")
library("posterior")

fit_vader_bantyp_antal.random <- read_rds("fit/fit_vader_bantyp_antal_random_uppdaterad.rds")

driver_vader_bantyp_antal.random_uppdaterad <- as_draws_df(fit_vader_bantyp_antal.random, variable = "r_driver\\[.+Intercept]", regex = TRUE) %>% select(-.chain, -.iteration)
driver_form_vader_bantyp_antal.random_uppdaterad <- as_draws_df(fit_vader_bantyp_antal.random, variable = "r_driver:year\\[.+Intercept]", regex = TRUE) %>% select(-.chain,-.iteration)

driver_mean_long_vader_bantyp_antal.random_uppdaterad <-
  driver_vader_bantyp_antal.random_uppdaterad  %>%
  pivot_longer(-.draw, names_to = "Driver", values_to = "Skill",
               names_pattern = "\\[(\\w+),") %>%
  mutate(Driver = as_factor(Driver))

driver_form_long_vader_bantyp_antal.random_uppdaterad <-
  driver_form_vader_bantyp_antal.random_uppdaterad %>%
  pivot_longer(-.draw, names_to = c("Driver", "Year"), values_to = "Form",
               names_pattern = "\\[(\\w+)_([0-9]{4}),") %>%
  mutate(Driver = as_factor(Driver), Year = as.integer(Year))

driver_samples_vader_bantyp_antal.random_uppdaterad <-
  left_join(driver_form_long_vader_bantyp_antal.random_uppdaterad, driver_mean_long_vader_bantyp_antal.random_uppdaterad, by = c("Driver", ".draw")) %>%
  mutate(skill_yr = Form + Skill)

driver_skill_summary_vader_bantyp_antal.random_uppdaterad <-
  driver_samples_vader_bantyp_antal.random_uppdaterad %>%
  group_by(Driver, Year) %>%
  summarise(
    est = mean(skill_yr),
    lower = quantile(skill_yr, 0.025),
    upper = quantile(skill_yr, 0.975),
    var = var(skill_yr),
    n = n(),
  )
write_rds(driver_skill_summary_vader_bantyp_antal.random_uppdaterad, "fit/summary_koef_driver_race_vader_bantyp_antal.random_uppdaterad.rds")
write_rds(driver_samples_vader_bantyp_antal.random_uppdaterad, "fit/samples_koef_driver_race_vader_bantyp_antal.random_uppdaterad.rds")

driver_samples_vader_bantyp_antal.random_uppdaterad      <- read_rds("fit/samples_koef_driver_race_vader_bantyp_antal.random_uppdaterad.rds")
driver_summary_vader_bantyp_antal.random_uppdaterad      <- read_rds("fit/summary_koef_driver_race_vader_bantyp_antal.random_uppdaterad.rds")




koef_driver_race_vader_bantyp_antal.random_uppdaterad           <- read_rds("fit/summary_koef_driver_race_vader_bantyp_antal.random_uppdaterad.rds") #Hämta koefficienterna för kval
koef_team_race_vader_bantyp_antal.random_uppdaterad              <- read_rds("fit/summary_koef_driver_race_vader_bantyp_antal.random_uppdaterad.rds")
unika_forare_race_vader_bantyp_antal.random_uppdaterad           <- as.character(unique(koef_driver_race_vader_bantyp_antal.random_uppdaterad$Driver))
koef_driver_race_vader_bantyp_antal.random_uppdaterad$Driver     <- as.character(koef_driver_race_vader_bantyp_antal.random_uppdaterad$Driver)
koef_driver_race_sort_vader_bantyp_antal.random_uppdaterad       <- koef_driver_race_vader_bantyp_antal.random_uppdaterad[koef_driver_race_vader_bantyp_antal.random_uppdaterad$Driver %in% unika_forare_race_vader_bantyp_antal.random_uppdaterad, ]
koef_driver_race_sort_vader_bantyp_antal.random_uppdaterad$Driver<- as.factor(koef_driver_race_sort_vader_bantyp_antal.random_uppdaterad$Driver)


## Sortering av data f?r plot
driver_skill_summary_sort_vader_bantyp_antal.random_uppdaterad <- arrange(koef_driver_race_sort_vader_bantyp_antal.random_uppdaterad, Driver, Year)
driver_skill_summary_sort_vader_bantyp_antal.random_uppdaterad <- driver_skill_summary_sort_vader_bantyp_antal.random_uppdaterad %>%
  group_by(Driver) %>%
  filter(n() > 2) # Best?m filterv?rde f?r hur m?nga s?songer en m?ste ha k?rt f?r att f? ett v?rde 


# Plot av peak skicklighet
driver_skill_peak_vader_bantyp_antal.random_uppdaterad <- driver_skill_summary_sort_vader_bantyp_antal.random_uppdaterad %>% group_by(Driver) %>% slice_max(n = 1, est) %>% arrange(desc(est))
driver_skill_peak_plot_vader_bantyp_antal.random_uppdaterad <- driver_skill_peak_vader_bantyp_antal.random_uppdaterad[1:15, ] %>%
  ungroup %>%
  mutate(Driver = fct_reorder(Driver, est)) %>%
  ggplot(aes(y = Driver, x = est, xmin = lower, xmax = upper)) +
  geom_pointrange(colour = "red") + 
  labs(title = "F1-förarskicklighet, peak",
       x = "Skill (log odds ratio)",
       y = "Driver") +
  theme_bw() +
  theme(text = element_text(size = 22, family = "serif"))
ggsave("img/driver_skill_peak_vader_bantyp_antal.random_uppdaterad.png", plot = driver_skill_peak_plot_vader_bantyp_antal.random_uppdaterad, width = 9, height = 6, bg = "white")
plot(driver_skill_peak_plot_vader_bantyp_antal.random_uppdaterad)
xtable::xtable(driver_skill_peak_vader_bantyp_antal.random_uppdaterad[1:15, ])



MA_peak_race      <- koef_driver_race_sort_vader_bantyp_antal.random_uppdaterad %>% group_by(Driver) %>% filter(n() >= 3) %>% mutate(MA = runMean(est, 3)) %>% slice_max(n = 1, MA) %>% arrange(desc(MA))
driver_skill_race_MA_peak_plot <- MA_peak_race[1:15, ] %>%
  ungroup %>%
  mutate(Driver = fct_reorder(Driver, MA)) %>%
  ggplot(aes(y = Driver, x = MA, xmin = lower, xmax = upper)) +
  geom_text(aes(label=Year+2), hjust=0, vjust=1.5, size=3)+
  geom_text(aes(label=Year), hjust=1.5, vjust=1.5, size=3)+
  geom_pointrange(colour = "red") + theme(axis.title = element_text()) +
  labs(title = "F1-förarskicklighet  3 år MA",
       subtitle = "Med hänsyn till konstruktörskapacitet, topp 15",
       x = "Skill (log odds ratio)",
       y = "Driver")

driver_skill_race_MA_peak_plot
ggsave("img/driver_skill_race_peak_MA_topp15.png", plot = driver_skill_race_MA_peak_plot, bg = "white")

MA_peak_race.ny <- MA_peak_race[order(MA_peak_race$est),]
xtable::xtable(MA_peak_race.ny[262:248, ])


fit_basic <- read_rds("fit/fit_basic_antal.random_13.rds")
fit_vader_bantyp_antal.rondom_13 <- read_rds("fit/fit_vader_bantyp_antal.random.rds")

sfit <- summary(fit_vader_bantyp_antal.rondom_13, prob = 0.95)
ranef_summary <- rbind(
  "constructor form" = sfit$random$`constructor:year`,
  "driver" = sfit$random$driver,
  "driver form" = sfit$random$`driver:year`,
  "antal" = sfit$random$antal,
)[c(1:8), 1:4]
sfit
ranef_summary
xtable::xtable(ranef_summary)




sfit_b <- summary(fit_basic, prob = 0.95)
ranef_summary_b <- rbind(
  "constructor form" = sfit_b$random$`constructor:year`,
  "driver" = sfit_b$random$driver,
  "driver form" = sfit_b$random$`driver:year`,
  "antal" = sfit_b$random$antal
)[c(1:4), 1:4]
sfit_b
ranef_summary_b
xtable::xtable(ranef_summary_b)

# how much of variance is due to car?
colSums(ranef_summary_b[1,]^2)/colSums(ranef_summary_b^2)

# and how much due to the driver?
colSums(ranef_summary_b[2:3,]^2)/colSums(ranef_summary_b^2)

# and how much of variance is due to number of finisher
colSums(ranef_summary_b[4,]^2)/colSums(ranef_summary_b^2)


####### Bästa konstruktör ######

constructor_form_vader_bantyp_antal.random_uppdaterad <- as_draws_df(fit_vader_bantyp_antal.rondom_13, variable = "r_constructor:year\\[.+Intercept]", regex = TRUE) %>% select(-.chain,-.iteration)

constructor_form_long_vader_bantyp_antal.random_uppdaterad <-
  constructor_form_vader_bantyp_antal.random_uppdaterad %>%
  pivot_longer(-.draw, names_to = c("constructor", "Year"), values_to = "advantage",
               names_pattern = "\\[(\\w+)_([0-9]{4}),") %>%
  mutate(constructor = as_factor(constructor), Year = as.integer(Year))

constructor_skill_summary_vader_bantyp_antal.random_uppdaterad <-
  constructor_form_long_vader_bantyp_antal.random_uppdaterad %>%
  group_by(constructor, Year) %>%
  summarise(
    est = mean(advantage),
    lower = quantile(advantage, 0.025),
    upper = quantile(advantage, 0.975),
    var = var(advantage),
    n = n(),
  )

write_rds(constructor_skill_summary_vader_bantyp_antal.random_uppdaterad, "fit/summary_koef_constructor_race_vader_bantyp_antal.random_uppdaterad.rds")
write_rds(constructor_form_long_vader_bantyp_antal.random_uppdaterad, "fit/samples_koef_constructor_race_vader_bantyp_antal.random_uppdaterad.rds")

constructor_samples_vader_bantyp_antal.random_uppdaterad      <- read_rds("fit/samples_koef_constructor_race_vader_bantyp_antal.random_uppdaterad.rds")
constructor_summary_vader_bantyp_antal.random_uppdaterad      <- read_rds("fit/summary_koef_constructor_race_vader_bantyp_antal.random_uppdaterad.rds")




koef_constructor_race_vader_bantyp_antal.random_uppdaterad                  <- read_rds("fit/summary_koef_constructor_race_vader_bantyp_antal.random_uppdaterad.rds") #Hämta koefficienterna för kval
koef_team_race_vader_bantyp_antal.random_uppdaterad                         <- read_rds("fit/summary_koef_constructor_race_vader_bantyp_antal.random_uppdaterad.rds")
unika_constructor_race_vader_bantyp_antal.random_uppdaterad                 <- as.character(unique(koef_constructor_race_vader_bantyp_antal.random_uppdaterad$constructor))
koef_constructor_race_vader_bantyp_antal.random_uppdaterad$constructor           <- as.character(koef_constructor_race_vader_bantyp_antal.random_uppdaterad$constructor)
koef_constructor_race_sort_vader_bantyp_antal.random_uppdaterad             <- koef_constructor_race_vader_bantyp_antal.random_uppdaterad[koef_constructor_race_vader_bantyp_antal.random_uppdaterad$constructor %in% unika_constructor_race_vader_bantyp_antal.random_uppdaterad, ]
koef_constructor_race_sort_vader_bantyp_antal.random_uppdaterad$constructor <- as.factor(koef_constructor_race_sort_vader_bantyp_antal.random_uppdaterad$constructor)

## Sortering av data f?r plot
constructor_skill_summary_sort_vader_bantyp_antal.random_uppdaterad <- arrange(koef_constructor_race_sort_vader_bantyp_antal.random_uppdaterad, constructor, Year)
constructor_skill_summary_sort_vader_bantyp_antal.random_uppdaterad <- constructor_skill_summary_sort_vader_bantyp_antal.random_uppdaterad %>%
  group_by(constructor) %>%
  filter(n() > 2) # Best?m filterv?rde f?r hur m?nga s?songer en m?ste ha k?rt f?r att f? ett v?rde 


# Plot av peak skicklighet
constructor_skill_peak_vader_bantyp_antal.random_uppdaterad <- constructor_skill_summary_sort_vader_bantyp_antal.random_uppdaterad %>% group_by(constructor) %>% slice_max(n = 1, est) %>% arrange(desc(est))
constructor_skill_peak_plot_vader_bantyp_antal.random_uppdaterad <- constructor_skill_peak_vader_bantyp_antal.random_uppdaterad[1:10, ] %>%
  ungroup %>%
  mutate(constructor = fct_reorder(constructor, est)) %>%
  ggplot(aes(y = constructor, x = est, xmin = lower, xmax = upper)) +
  geom_text(aes(label=Year), hjust=1.5, vjust=1.5, size=3) +
  geom_pointrange(colour = "red") + 
  labs(title = "F1-Stall  peak",
       x = "Skill (log odds ratio)",
       y = "Stall")
ggsave("img/constructor_skill_peak_vader_bantyp_antal.random_uppdaterad.png", plot = constructor_skill_peak_plot_vader_bantyp_antal.random_uppdaterad, width = 9, height = 9, bg = "white")
plot(constructor_skill_peak_plot_vader_bantyp_antal.random_uppdaterad)
xtable::xtable(constructor_skill_peak_vader_bantyp_antal.random_uppdaterad[1:10, ])


##### MA constructor
MA_peak_race_constructor      <- koef_constructor_race_sort_vader_bantyp_antal.random_uppdaterad %>% group_by(constructor) %>% filter(n() >= 3) %>% mutate(MA = runMean(est, 3)) %>% slice_max(n = 1, MA) %>% arrange(desc(MA))
constructor_skill_race_MA_peak_plot <- MA_peak_race_constructor[1:10, ] %>%
  ungroup %>%
  mutate(constructor = fct_reorder(constructor, est)) %>%
  ggplot(aes(y = constructor, x = est, xmin = lower, xmax = upper)) +
  geom_pointrange(colour = "red") + theme(axis.title = element_text()) +
  labs(title = "F1-stall  3 år MA",
       x = "Skill (log odds ratio)",
       y = "Stall")

constructor_skill_race_MA_peak_plot
ggsave("img/constructor_skill_race_peak_MA_topp15.png", plot = constructor_skill_race_MA_peak_plot, bg = "white")
MA_peak_race_constructor.ny <- MA_peak_race_constructor[order(MA_peak_race_constructor$est),]
xtable::xtable(MA_peak_race_constructor.ny[262:248, ])
xtable(MA_peak_race_constructor[1:15], )


fit_vader_bantyp_antal.random <- read_rds("fit/fit_vader_bantyp_antal_random_uppdaterad.rds")

data <- read_rds("dat/f1_dat_finished.rds")

prop <- data$proportional_trans

postrior <- posterior_predict(fit_vader_bantyp_antal.random,
                              draws = 7000) 

color_scheme_set("blue")

ppc_dens_overlay(prop,
                 postrior)



