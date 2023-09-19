setwd("~/Library/CloudStorage/OneDrive-Privat/Final modell 2.0")
library(tidyverse)
library(lubridate)
library(jsonlite)
library(glue)
library(rvest)

tab_races        <- read_csv("dat/f1db_csv/races.csv")
tab_circuits     <- read_csv("dat/f1db_csv/circuits.csv")
# Results information
tab_results      <- read_csv("dat/f1db_csv/results.csv")
tab_drivers      <- read_csv("dat/f1db_csv/drivers.csv")
tab_constructors <- read_csv("dat/f1db_csv/constructors.csv")
tab_status       <- read_csv("dat/f1db_csv/status.csv")
tab_qualifying   <- read_csv("dat/f1db_csv/qualifying.csv") 
tab_seasons      <- read_csv("dat/f1db_csv/seasons.csv")
race_info        <- read_rds("dat/race_info.rds")



## Select columns ----
race_dat <- race_info %>%
  select(raceId, year, round, circuitRef, country, weather_type, circuit_type, raceId)
## Manually add missing circuit info ----
circuit_missing_idx <- which(is.na(race_dat$circuit_type))
circuit_type_missing <- race_dat[c(circuit_missing_idx), ] 
which(is.na(race_dat$circuit_type))

vilka_banor <- unique(circuit_type_missing$circuitRef)
# "dijon = permanent"
# "zolder = permanent"
# "anderstopr = permanent"


race_dat[circuit_missing_idx, "circuit_type"] <- c(
  "permanent", # dijon 1982
  "permanent", # zolder 1982
  "permanent", # zolder 1980
  "permanent", # zolder 1979
  "permanent", # dijon  1979
  "permanent", # zolder 1978
  "permanent", # zolder 1977
  "permanent", # dijon  1977
  "permanent", # zolder 1976
  "permanent", # anderstorp  1976
  "permanent", # zolder 1975
  "permanent" # anderstorp  1975
)


# Result information ----
results_dat <-
  tab_results %>%
  left_join(tab_drivers, by = "driverId") %>%
  left_join(tab_constructors, by = "constructorId") %>%
  left_join(tab_status, by = "statusId") %>%
  select(raceId, positionText, positionOrder, grid, fastestLapTime, driverRef, constructorRef, status, milliseconds, laps, raceId)

# Joining & cleaning ----
f1_dat <-
  race_dat %>%
  left_join(results_dat, by = "raceId") %>%
  rename(circuit = circuitRef, driver = driverRef, constructor = constructorRef,
         position = positionOrder, fastest_lab = fastestLapTime) %>%
  select(driver, constructor, year, round, circuit, position, grid, weather_type, circuit_type, milliseconds, laps, status, raceId) %>%
  mutate(year = as.integer(year), round = as.integer(round), position = as.integer(position), grid = as.integer(grid), laps = as.integer(laps), milliseconds = as.numeric(milliseconds))


# Dubbelkollar nedan så att det ej finns NA
which(is.na(f1_dat$circuit_type))
which(is.na(f1_dat$weather_type))
which(is.na(f1_dat$driver))
which(is.na(f1_dat$constructor))
which(is.na(f1_dat$year))
which(is.na(f1_dat$round))
which(is.na(f1_dat$circuit))
which(is.na(f1_dat$position))
which(is.na(f1_dat$circuit_type))
which(is.na(f1_dat$milliseconds))

write_rds(f1_dat, "dat/f1_dat.rds")
