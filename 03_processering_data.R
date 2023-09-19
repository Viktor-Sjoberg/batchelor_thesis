
setwd("C:/Users/claes/OneDrive/Universitet/Statistik Fortsättningskurs/STAH11 Kandidatuppsats/Final modell 2.0")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(viridis)


f1_dat <- read_rds("dat/f1_dat.rds")
f1_dat_kval <- read_rds("dat/f1_dat_kval.rds")

f1_dat_finished <- read_rds("dat/f1_dat_finished.rds")

# Data processering ----

##### Data processering pa RACE -----
# Konvertera till faktorer
f1_dat <- f1_dat %>% mutate(
  status       = as_factor(status),
  constructor  = as_factor(constructor),
  driver       = as_factor(driver),
  weather_type = as_factor(weather_type),
  circuit_type = as_factor(circuit_type)
)

f1_dat$t10 <- ifelse(f1_dat$position>10, 11, f1_dat$position)
f1_dat$t13 <- ifelse(f1_dat$position>13, 14, f1_dat$position)
f1_dat$t15 <- ifelse(f1_dat$position>15, 16, f1_dat$position)
f1_dat$t20 <- ifelse(f1_dat$position>20, 21, f1_dat$position)
f1_dat$t13 <- ifelse(f1_dat$position>13, 14, f1_dat$position)

# Exkludera icke-klassificerade
compute_classified <- function(status) {
  out <- rep("not classified", length(status))
  # anyone above the last person still running (finished or +n laps is classified)
  last_classified <- max(which(status == "Finished" | str_starts(status, "\\+")))
  out[1:last_classified] <- "classified"
  out
}


f1_dat_finished <-
  f1_dat %>%
  group_by(year, round) %>%
  arrange(position, .by_group = TRUE) %>%
  mutate(classified = compute_classified(status)) %>%
  filter(classified == "classified" & circuit !="indianapolis") %>%
  mutate(
    antal = (n()),
    rank_pos = rank(position, ties.method= "random"),
    position_prop = (n() - rank_pos) / (n() - 1),         # how many classified drivers did you beat?
    prop_trans = (position_prop * (n() - 1) + 0.5) / n(), # https://stats.stackexchange.com/a/134297/116878
    grid_prop = (n() - rank(grid)) / (n()-1),
    gridprop_trans = (grid_prop * (n() - 1)+0.5) / n(),
    netto.grid = rank(grid),
    milliseconds2 = ifelse( (max(laps)-laps) > 0, max(milliseconds, na.rm = TRUE) + (max(milliseconds, na.rm = TRUE)/max(laps) * ( (max(laps, na.rm = TRUE)-laps)-1) ), milliseconds),
    milli_intervall =  max(milliseconds2, na.rm = TRUE) - (milliseconds2),         # +1 justering för att inte dela med noll
    prop_milli = milli_intervall / (max(milli_intervall, na.rm = TRUE)),
    propmilli_trans = (prop_milli * (n() - 1) + 0.5) / n(),
    position_prop_10 = (11 - t10) / (11 - 1),        # how many classified drivers did you beat?
    position_prop_13 = (14 - t13) / (14 - 1),
    position_prop_15 = (16 - t15) / (16 - 1),
    position_prop_20 = (21 - t20) / (21 - 1), # https://stats.stackexchange.com/a/134297/116878
    position_prop_13 = (14 - t13) / (14 - 1),
    prop_trans_10 = (position_prop_10 * (11 - 1) + 0.5) / 11,
    prop_trans_13 = (position_prop_13 * (14 - 1) + 0.5) / 14,
    prop_trans_15 = (position_prop_15 * (16 - 1) + 0.5) / 16,
    prop_trans_20 = (position_prop_20 * (21 - 1) + 0.5) / 21,
    prop_trans_13 = (position_prop_13 * (14 - 1) + 0.5) / 14
  ) %>%
  ungroup() %>%
  select(-classified)


f1_dat_finished <-
  f1_dat %>%
  group_by(year, round) %>%
  arrange(position, .by_group = TRUE) %>%
  mutate(classified = compute_classified(status)) %>%
  filter(classified == "classified" & circuit !="indianapolis") %>%
  mutate(
    antal = (n()),
    rank_pos = rank(position, ties.method= "random"),
    position_prop = (n() - rank_pos) / (n() - 1),         # how many classified drivers did you beat?
    prop_trans = (position_prop * (n() - 1) + 0.5) / n(),
    pos_prop = ifelse(position <= 13,  (14 - position) / (14 - 1), 0.076923077* (0.1^0.2)^(position-13) ),
    proportional_trans = (pos_prop * (14 - 1) + 0.5) / 14)

f1_dat_finished <- 
  f1_dat_finished %>% drop_na(prop_trans)

write_rds(f1_dat_finished, "dat/f1_dat_finished.rds")


#Dubbelkolla så att det inte finns NA på viktiga kolumner
which(is.na(f1_dat_finished$circuit_type))
which(is.na(f1_dat_finished$prop_trans)) 
which(is.na(f1_dat_finished$gridprop_trans))
write_rds(f1_dat_finished, "dat/f1_dat_finished.rds")


##### Data Processering på KVAL -----
f1_dat_kval <- f1_dat_kval %>% mutate(
  constructor  = as_factor(constructor),
  driver       = as_factor(driver),
  circuit_type = as_factor(circuit_type)
)

f1_dat_kval_bearbetad <-
  f1_dat_kval %>%
  group_by(year, round) %>%
  mutate(
    kvalposition_prop = (n() - position) / (n() - 1),         # how many classified drivers did you beat?
    prop_trans = (kvalposition_prop * (n() - 1) + 0.5) / n()  # https://stats.stackexchange.com/a/134297/116878
  ) %>%
  ungroup()
f1_dat_kval_bearbetad <- f1_dat_kval_proc[f1_dat_kval_proc$kvalposition_prop >= 0, ]
which(is.na(f1_dat_kval_bearbetad$prop_trans)) # Måste vara 0.
write_rds(f1_dat_kval_bearbetad, "dat/f1_dat_kval_bearbetad.rds")





##### Lite deskriptiv statistik -----

### Antal lopp per år

t <- f1_dat %>% group_by(year) %>% 
  summarise(antal = n_distinct(raceId)) %>% 
  ggplot(aes(x = year, y = antal)) +
  geom_bar(stat = "identity") +
  labs(title = "Antal lopp per år", x = "År", y = "Antal") + 
  theme_bw() +
  theme(plot.title = element_text(size = 24, family = "serif"),
        plot.subtitle = element_text(size = 18, family = "serif"),
        
        axis.title.x = element_text(size = 16, family = "serif"),
        axis.title.y = element_text(size = 16, family = "serif"),
        axis.text = element_text(size = 9, colour = "black", family = "serif"),
        axis.line = element_line(linewidth = 2, colour = "lightblue"))
ggsave("desk_img/antal_lopp_per_ar.png", width = 6, height = 6)



# finish position ,    (!!!!!!!) Notera att det finns bortfall i data, speciellt av vinnare
ggplot(f1_dat_finished, aes(x = factor(position))) +
  geom_bar() + theme_fivethirtyeight() + theme(axis.title = element_text()) + 
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) +
  labs(
    title = "Fördelning av slutpositioner",
    x = "Slutposition",
    y = "Antal"
  ) + guides(color = FALSE)
ggsave("img/fordelning_slutposition.png", width = 9, height = 6)

# finish position
ggplot(f1_dat_kval[f1_dat_kval$year > 1982, ], aes(x = factor(position))) +
  geom_bar() +
  labs(
    title = "Fördelning av kvalposition",
    x = "Kvalposition",
    y = "Antal"
  )
ggsave("img/fordelning_kvalposition.png", width = 9, height = 6)


### basic plot 
# (2019-2021)
f1_dat_finished[f1_dat_finished$year > 2018, ] %>%
  filter(driver %in% c("hamilton", "raikkonen", "giovinazzi")) %>%
  ggplot(aes(x = factor(position), fill = driver)) +
  geom_bar(position = position_dodge(preserve = "single")) + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) + 
  labs(
    x = "Slutposition",
    y = "Antal",
    title = "Olika förarers slutposition (2019-2021)",
    subtitle = "Givet att förare inte brutit",
    fill = ""
  ) +
  theme(legend.position = "top") +
  facet_wrap(~year)
ggsave("img/slutposition_2019_2021.png", width = 12, height = 9)

# (1998-2000)
f1_dat_finished[f1_dat_finished$year > 1997 & f1_dat_finished$year < 2001, ] %>%
  filter(driver %in% c("michael_schumacher", "hakkinen", "barrichello")) %>%
  ggplot(aes(x = factor(position), fill = driver)) +
  geom_bar(position = position_dodge(preserve = "single")) + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) +
  labs(
    x = "Slutposition",
    y = "Antal",
    title = "Olika förarers slutposition (1998-2000)",
    subtitle = "Givet att förare inte brutit",
    fill = ""
  ) +
  theme(legend.position = "top") +
  facet_wrap(~year)
ggsave("img/slutposition_1998_2000.png", width = 12, height = 9)


### Basic plot density över andel outperformed
# År (2018-2021)
f1_dat_finished[f1_dat_finished$year > 2018, ] %>%
  filter(driver %in% c("hamilton", "raikkonen", "giovinazzi")) %>%
  ggplot(aes(x = prop_trans, fill = driver)) +
  geom_density(alpha = 0.5, bw = 0.1) + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) +
  labs(
    x = "Andel slagna förare",
    y = "Densitet",
    title = "Givna förares resultat 2019-2021",
    subtitle = "Utjämnad andel av antalet slagna förare",
    fill = ""
  ) +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, vjust = 0.85)) +
  facet_wrap(~year)
ggsave("img/densitet_outperformed_2018_2021.png", width = 12, height = 9)

# År (1998-2001)
f1_dat_finished[f1_dat_finished$year > 1997 & f1_dat_finished$year < 2001, ] %>%
  filter(driver %in% c("michael_schumacher", "hakkinen", "barrichello")) %>%
  ggplot(aes(x = prop_trans, fill = driver)) +
  geom_density(alpha = 0.5, bw = 0.1) + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) +
  labs(
    x = "Andel slagna förare",
    y = "Densitet",
    title = "Givna förares resultat 1998-2000",
    subtitle = "Utjämnad andel av antalet slagna förare",
    fill = ""
  ) +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, vjust = 0.85)) +
  facet_wrap(~year)
ggsave("img/densitet_outperformed_1998_2000.png", width = 12, height = 9)


# 1955-1957
f1_dat_finished[f1_dat_finished$year > 1954 & f1_dat_finished$year < 1958, ] %>%
  filter(driver %in% c("hawthorn", "fangio", "piotti")) %>%
  ggplot(aes(x = prop_trans, fill = driver)) +
  geom_density(alpha = 0.5, bw = 0.1) + theme_fivethirtyeight() + theme(axis.title = element_text()) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) +
  labs(
    x = "Andel slagna förare",
    y = "Densitet",
    title = "Givna förares resultat 1955-1957",
    subtitle = "Utjämnad andel av antalet slagna förare",
    fill = ""
  ) +
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, vjust = 0.85)) +
  facet_wrap(~year)
ggsave("img/densitet_outperformed_1955_1957.png", width = 12, height = 9)
