# -------------------------------------
# Script: 

# Author: Claes Croneborg

# Purpose: 

# Notes:

# License: MIT

# Encoding: UTF-8-SV
Sys.setlocale("LC_ALL", "sv_SE.UTF-8")

# Packages:
library(tidyverse)
library(readr)
library(readxl)
library(ggthemes)

# -------------------------------------
f1_dat <- read_rds("dat/f1_dat.rds")
f1_dat_finished <- read_rds("dat/f1_dat_finished.rds")

results                 <- read_csv("dat/f1db_csv/results.csv")
races                   <- read_csv("dat/f1db_csv/races.csv")
circuits                <- read_csv("dat/f1db_csv/circuits.csv")
drivers                 <- read_csv("dat/f1db_csv/drivers.csv")
driversStandings        <- read.csv("dat/f1db_csv/driver_standings.csv")
constructors            <- read_csv("dat/f1db_csv/constructors.csv")
constructorStandings    <- read_csv("dat/f1db_csv/constructor_standings.csv")
constructorResults      <-read_csv("dat/f1db_csv/constructor_results.csv")





##### Antal lopp över alla säsonger -----
antal_lopp <- f1_dat %>% group_by(year) %>% 
  summarise(antal = n_distinct(raceId)) %>% 
  ggplot(aes(x = year, y = antal)) +
  geom_bar(stat = "identity") +
  scale_x_continuous("Årtal", breaks = seq(1950,2021, 10)) +
  labs(title = "Antal lopp per år", x = "Årtal", y = "Antal") + 
  theme_bw() +
  theme(text = element_text(size = 22, family = "serif"),
        axis.line = element_line(linewidth = 2, colour = "lightblue"))
antal_lopp
ggsave("desk_img/antal_lopp_per_ar.png", plot = antal_lopp, width = 9, height = 6,  bg = "white")

##### Plott av antal vinster och vinstprocent för förare och team -----

vinst_och_vinstprocent_forare <- function(antal_forare){
  forare_radata <- left_join(read_csv("dat/f1db_csv/results.csv"), read_csv("dat/f1db_csv/drivers.csv") %>% select(-url), by = "driverId")
  forare_summering <<- forare_radata %>%
    mutate(full_name = paste(forename, surname)) %>% 
    group_by(full_name) %>%
    mutate(vinst = ifelse(position == 1, 1, 0)) %>%
    summarise(antal_race = n(),
              antal_vinster = sum(vinst),
              vinstprocent = antal_vinster / antal_race) %>%
    slice_max(order_by = antal_vinster, n=antal_forare, with_ties = FALSE)
  
  plot_forare <- ggplot(forare_summering, aes(x=reorder(full_name, antal_vinster), y = antal_vinster)) +
    geom_col() +
    geom_point(aes(y = vinstprocent*100), color = "red", size = 3) +
    scale_y_continuous("Antal vinster per förare", limits = c(0, 105),sec.axis = sec_axis(~., name = "Vinstprocent % per förare"))  +
    ggtitle("Vinster och vinstprocent per förare") + xlab("Förare") +
    coord_flip() + theme_bw() + 
    theme(text = element_text(size = 22, family = "serif"),
          axis.line.x.top = element_line(color = "red"), 
          axis.ticks.x.top = element_line(color = "red"),
          axis.text.x.top = element_text(color = "red"), 
          axis.title.x.top = element_text(color = "red"),
          axis.line = element_line(linewidth = 1, colour = "lightblue"))
  plot_forare
  ggsave("desk_img/antal_vinster_vinstprocent_forare.png", plot = plot_forare, width = 9, height = 5.5, bg = "white")
}
vinst_och_vinstprocent_forare(12)


### Konstuktör
vinst_och_vinstprocent_konstruktor <- function(antal_team){
  
  konstruktor_summering <<- left_join(read_csv("dat/f1db_csv/results.csv"), read_csv("dat/f1db_csv/constructors.csv") %>% select(-url), by = "constructorId") %>%
    mutate(vinst = ifelse(positionOrder == 1, 1, 0)) %>% group_by(raceId, name) %>%
    summarise(antal = sum(vinst)) %>%
    group_by(name) %>%
    summarise(antal_race = n(),
              antal_vinster = sum(antal),
              vinstprocent = antal_vinster / antal_race) %>%
    slice_max(order_by = antal_vinster, n = antal_team, with_ties = FALSE)
  
  plot_konstruktor <- ggplot(konstruktor_summering, aes(x=reorder(name, antal_vinster), y = antal_vinster)) +
    geom_col() +
    geom_point(aes(y = vinstprocent*250), color = "red", size = 3) +
    scale_y_continuous("Antal vinster per konstruktör", limits = c(0, 250),sec.axis = sec_axis(~./2.5, name = "Vinstprocent % per konstruktör")) +
    ggtitle("Vinster och vinstprocent per konstruktör") + xlab("Konstruktör") +
    coord_flip() + theme_bw() + 
    theme(text = element_text(size = 22, family = "serif"),
          axis.line.x.top = element_line(color = "red"), 
          axis.ticks.x.top = element_line(color = "red"),
          axis.text.x.top = element_text(color = "red"), 
          axis.title.x.top = element_text(color = "red"),
          axis.line = element_line(linewidth = 1, colour = "lightblue"))
  plot(plot_konstruktor)
  ggsave("desk_img/antal_vinster_vinstprocent_konstruktor.png", plot = plot_konstruktor, width = 9, height = 5.5, bg = "white")
}
vinst_och_vinstprocent_konstruktor(10)





antalimal <- left_join(races[,1:2], results, by = "raceId") %>% 
  filter(year <= 2022) %>%
  group_by(raceId) %>%
  mutate(inte_med_i_race = ifelse(positionText == "E" | positionText == "F" | positionText == "N" | positionText == "W",1, 0),
         antal = n() - sum(inte_med_i_race))  %>%
  filter ( inte_med_i_race == 0) %>%
  group_by(year) %>%
  mutate(classified = ifelse(positionText == "R" | positionText == "D", 0, 1)) %>%
  summarise(medel = mean(antal),
            procentimal = ( (sum(classified) / n())*100 ),
            genomsnittantalimal = medel * (procentimal/100)) %>%
  mutate(korrektion = max(genomsnittantalimal)  / genomsnittantalimal)

antalimal_plot  <- ggplot(antalimal, aes(year, medel)) +
  geom_col() +
  geom_line(aes(y = procentimal/2.86), color = "red", lwd = 1.5) +
  scale_y_continuous("Antal förare", limit = c(0, 35), sec.axis = sec_axis(~ . /0.35, name = "Procent % i mål")) +
  scale_x_continuous("Årtal", breaks = seq(1950,2021, 10)) +
  ggtitle("Genomsnittligt antal förare som startar", subtitle = "Med genomsnittlig procent som går i mål") + 
  theme_bw() + 
  theme(text = element_text(size = 22, family = "serif"),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red"),
        axis.line = element_line(linewidth = 2, colour = "lightblue"))
ggsave("desk_img/genomsnitt_forare_i_mal.png", plot = antalimal_plot, width = 9, height = 6, bg = "white")
plot(antalimal_plot)





antal_i_mal_plot <- f1_dat_finished %>% 
  group_by(year, round) %>% 
  summarise(antal = n()) %>% 
  ggplot(aes(x = year, y = antal, group = year)) + 
  geom_boxplot() +
  geom_smooth(aes(group = 1), method = "loess") +
  labs(title = "Antal förare som kommer i mål", x = "Årtal", y = "Antal") + 
  scale_x_continuous("Årtal", breaks = seq(1950,2021, 10)) +
  theme_bw() + 
  theme(text = element_text(size = 22, family = "serif"),
        axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red"),
        axis.line = element_line(linewidth = 2, colour = "lightblue"))
ggsave("desk_img/boxplot_antal_forare_i_mal.png", plot = antal_i_mal_plot, width = 9, height = 6, bg = "white")
antal_i_mal_plot
