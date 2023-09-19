
setwd("C:/Users/claes/OneDrive/Universitet/Statistik Fortsättningskurs/STAH11 Kandidatuppsats/Final modell 2.0")


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



##### Sortering av simuleringar

# Väder + Bantyp är den bästa modellen

sortering_simuleringar_forare <- function() {
fit <- read_rds("fit/fit_vader_bantyp_antal_random_uppdaterad.rds")
driver_mean <- as_draws_df(fit, variable = "r_driver\\[.+Intercept]", regex = TRUE) %>% select(-.chain, -.iteration)
driver_form <- as_draws_df(fit, variable = "r_driver:year\\[.+Intercept]", regex = TRUE) %>% select(-.chain,-.iteration)

driver_mean_long <-
  driver_mean  %>%
  pivot_longer(-.draw, names_to = "Driver", values_to = "Skill",
               names_pattern = "\\[(\\w+),") %>%
  mutate(Driver = as_factor(Driver))

driver_form_long <-
  driver_form %>%
  pivot_longer(-.draw, names_to = c("Driver", "Year"), values_to = "Form",
               names_pattern = "\\[(\\w+)_([0-9]{4}),") %>%
  mutate(Driver = as_factor(Driver), Year = as.integer(Year))

driver_samples <<-
  left_join(driver_form_long, driver_mean_long, by = c("Driver", ".draw")) %>%
  mutate(skill_yr = Form + Skill)

driver_skill_summary <<-
  driver_samples %>%
  group_by(Driver, Year) %>%
  summarise(
    est = mean(skill_yr),
    lower = quantile(skill_yr, 0.025),
    upper = quantile(skill_yr, 0.975),
    var = var(skill_yr),
    n = n()
  )

write_rds(driver_skill_summary, "fit/summary_koef_driver_race.rds")
write_rds(driver_samples, "fit/samples_koef_driver_race.rds")
}


sortering_simuleringar_konstruktor <- function() {
  fit <- read_rds("fit/fit_vader_bantyp_antal_random_uppdaterad.rds")
  constructor_form <- as_draws_df(fit, variable = "r_constructor:year\\[.+Intercept]", regex = TRUE) %>% select(-.chain,-.iteration)
  
  constructor_samples <<-
    constructor_form %>%
    pivot_longer(-.draw, names_to = c("Constructor", "Year"), values_to = "Form",
                 names_pattern = "\\[(\\w+)_([0-9]{4}),") %>%
    mutate(Constructor = as_factor(Constructor), Year = as.integer(Year))
  
  constructor_advantage_summary <<-
    constructor_samples %>%
    group_by(Constructor, Year) %>%
    summarise(
      est = mean(Form),
      lower = quantile(Form, 0.055),
      upper = quantile(Form, 0.945),
      var = var(Form),
      n = n()
    )
  write_rds(constructor_advantage_summary, "fit/summary_koef_team_race_uppdaterad.rds")
  write_rds(constructor_samples, "fit/samples_koef_team_race_uppdaterad.rds")
}
