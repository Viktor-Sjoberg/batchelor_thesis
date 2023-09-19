
setwd("C:/Users/claes/OneDrive/Universitet/Statistik Fortsättningskurs/STAH11 Kandidatuppsats/Final modell 2.0")
library(tidyverse)

##### Plott av antal vinster och vinstprocent för förare och team -----

### Förare
vinst_och_vinstprocent_forare <- function(antal_forare){
  forare_radata <- left_join(read_csv("dat/f1db_csv/results.csv"), read_csv("dat/f1db_csv/drivers.csv") %>% select(-url), by = "driverId")
  forare_summering <<- forare_radata %>%
    group_by(driverRef) %>%
    mutate(vinst = ifelse(position == 1, 1, 0)) %>%
    summarise(antal_race = n(),
              antal_vinster = sum(vinst),
              vinstprocent = antal_vinster / antal_race) %>%
    slice_max(order_by = antal_vinster, n=antal_forare, with_ties = FALSE)
  
  plot_forare <- ggplot(forare_summering, aes(x=reorder(driverRef, antal_vinster), y = antal_vinster)) +
    geom_col() +
    geom_point(aes(y = vinstprocent*100), color = "red", lwd = 3) +
    scale_y_continuous("Antal vinster per förare", limits = c(0, 105),sec.axis = sec_axis(~., name = "Vinstprocent % per förare"))  +
    ggtitle("Summering antal vinster och vinstprocent per förare", subtitle = "Topp 15 Åren 1950 - 2022") + xlab("Förare") +
    coord_flip() + theme_classic() + 
    theme(axis.line.x.top = element_line(color = "red"), 
          axis.ticks.x.top = element_line(color = "red"),
          axis.text.x.top = element_text(color = "red"), 
          axis.title.x.top = element_text(color = "red"))
  plot_forare
  ggsave("desk_img/antal_vinster_vinstprocent_forare.png", plot = plot_forare, width = 9, height = 6, bg = "white")
}


### Konstuktör
vinst_och_vinstprocent_konstruktor <- function(antal_team){
  
  konstruktor_summering <<- left_join(read_csv("dat/f1db_csv/results.csv"), read_csv("dat/f1db_csv/constructors.csv") %>% select(-url), by = "constructorId") %>%
    mutate(vinst = ifelse(positionOrder == 1, 1, 0)) %>% group_by(raceId, constructorRef) %>%
    summarise(antal = sum(vinst)) %>%
    group_by(constructorRef) %>%
    summarise(antal_race = n(),
              antal_vinster = sum(antal),
              vinstprocent = antal_vinster / antal_race) %>%
    slice_max(order_by = antal_vinster, n = antal_team, with_ties = FALSE)
 
  plot_konstruktor <- ggplot(konstruktor_summering, aes(x=reorder(constructorRef, antal_vinster), y = antal_vinster)) +
    geom_col() +
    geom_point(aes(y = vinstprocent*250), color = "red", lwd = 3) +
    scale_y_continuous("Antal vinster per konstruktör", limits = c(0, 250),sec.axis = sec_axis(~./2.5, name = "Vinstprocent % per konstruktör")) +
    ggtitle("Summering antal vinster och vinstprocent per konstruktör", subtitle = "Topp 15 Åren 1950 - 2022") + xlab("Konstruktör") +
    coord_flip() + theme_classic() + 
    theme(axis.line.x.top = element_line(color = "red"), 
          axis.ticks.x.top = element_line(color = "red"),
          axis.text.x.top = element_text(color = "red"), 
          axis.title.x.top = element_text(color = "red"))
  plot(plot_konstruktor)
  ggsave("desk_img/antal_vinster_vinstprocent_konstruktor.png", plot = plot_konstruktor, width = 9, height = 6, bg = "white")
}















##### Plottning av deskriptiv statistik antal förare i mål -----
klassificerade <- function(status) {
  out <- rep(0, length(status))
  # anyone above the last person still running (finished or +n laps is classified)
  last_classified <- max(which(status == "Finished" | str_starts(status, "\\+")))
  out[1:last_classified] <- 1
  out
}
### Stapeldiagram med antal startande och antal i mål
barplot_antalimal <- function() {
  antalimal <<- read_rds("dat/f1_dat.rds") %>%
    filter(status != "Did not prequalify") %>%
    group_by(year, round) %>%
    arrange(position, .by_group = TRUE) %>%
    mutate(classified = klassificerade(status)) %>%
    summarise(antal = n(),
              antal_i_mal = sum(classified),
              antal_i_mal_procent = (antal_i_mal / antal)*100) %>%
    group_by(year) %>%
    summarise(medel = mean(antal),
              procent_i_mal = (sum(antal_i_mal) / sum(antal))*100)
  
  plottning <- ggplot(antalimal, aes(year, medel)) +
    geom_col() +
    geom_line(aes(y = procent_i_mal/2.86), color = "red", lwd = 1.5) +
    scale_y_continuous("Genomsnitt antal förare", limit = c(0, 35), sec.axis = sec_axis(~ . /0.35, name = "Procent % i mål")) +
    scale_x_continuous("Årtal", breaks = seq(1950,2022, 10)) +
    ggtitle("Genomsnittligt antal förare som startar, grupperat per race per År", subtitle = "Med genomsnittlig procent som gick i mål") + 
    theme_classic() + 
    theme(axis.line.y.right = element_line(color = "red"), 
          axis.ticks.y.right = element_line(color = "red"),
          axis.text.y.right = element_text(color = "red"), 
          axis.title.y.right = element_text(color = "red")
    )
  ggsave("desk_img/genomsnitt_forare_i_mal.png", plot = plottning, width = 9, height = 6, bg = "white")
}

### Boxplot med antal förare som kom i mål
boxplot_antalimal <- function() {
  f1_dat_finished <- read_rds("dat/f1_dat_finished.rds") 
  
  antalimal_boxplot <- f1_dat_finished %>% group_by(year, round) %>% summarise(antal = n()) %>% 
  ggplot(aes(x = factor(year), y = antal, fill = antal)) +
  geom_boxplot(alpha = .25) + theme_classic() +
  geom_smooth(method = 'loess', aes(group=1), span = 0.2, color = 'red', lty = 2, size = 0.1, alpha = .6) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20)))+ 
  scale_x_discrete(breaks=seq(min(f1_dat_finished$year),max(f1_dat_finished$year),10)) +
  labs(title = "Antal förare som tar målflagg per race",
       subtitle = "Grupperat per År") + ylab("Antal") + xlab("Årtal") +
  guides(color = FALSE)
antalimal_boxplot
ggsave("desk_img/antalimal_per_ar_boxplot.png", plot = antalimal_boxplot, width = 9, height = 6, bg = "white")
}
