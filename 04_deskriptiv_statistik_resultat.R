setwd("~/Library/CloudStorage/OneDrive-Privat/Final modell 2.0") 

setwd("~/Library/CloudStorage/OneDrive-Privat/Final modell 2.0")
#Deskriptiv dataanalys
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(ggrepel)
library(viridis)
library(circlize)
library(readr)
library(writexl)

##### Data inladdning #####
results                 <- read_csv("dat/f1db_csv/results.csv")
races                   <- read_csv("dat/f1db_csv/races.csv")
circuits                <- read_csv("dat/f1db_csv/circuits.csv")
drivers                 <- read_csv("dat/f1db_csv/drivers.csv")
driversStandings        <- read.csv("dat/f1db_csv/driver_standings.csv")
constructors            <- read_csv("dat/f1db_csv/constructors.csv")
constructorStandings    <- read_csv("dat/f1db_csv/constructor_standings.csv")
constructorResults      <-read_csv("dat/f1db_csv/constructor_results.csv")

race.s <- ggplot(data=races, aes(x=year, y=round/round)) + 
  geom_bar(stat="identity") +
  xlab("År") +
  ylab("Antal lopp")

race.s
ggsave("img/antal_lopp_per_ar", plot = race.s, width = 12, height = 6, bg = "white", device = "pdf")


##### Databearbetning #####
# Results
results$fastestLapSpeed <- as.numeric(results$fastestLapSpeed)
convertFastestLap <- function(x){
  if(length(x)>0){
    curMinute<-as.numeric(strsplit(x,":")[[1]][1])
    curSecond<-as.numeric(strsplit(strsplit(x,":")[[1]][2],"\\.")[[1]][1])
    return(curMinute*60 + curSecond)
  }
  else if(length(x)==0){
    return(NA)
  }
}
results$fastestLapTimeNum <- sapply(results$fastestLapTime, convertFastestLap)

# Races
races$date<-as.Date(races$date,"%Y-%m-%d")
races$name<-gsub(" Grand Prix","",races$name)
results_2<-left_join(
  results %>% dplyr::select(-time, -fastestLapTime), 
  races %>% dplyr::select(-time, -url), 
  by='raceId')

# Circuits
races <- left_join(races %>% select(-name,-url), circuits %>% select(-url), by='circuitId')




##### Statistik omk?rningar ?ver tid #####
results_omkorningar_1 <- results_2 
results_omkorningar_1$positionText <- as.numeric(results_omkorningar_1$positionText)  
results_omkorningar <- results_omkorningar_1 %>% drop_na(positionText)
summering_omkorningar <- results_omkorningar %>% arrange(name, grid) %>%
  group_by(raceId) %>% 
  mutate(rank.grid = rank(grid), rank.position= rank(positionOrder), netto.overtakes = rank(positionOrder)- rank(grid))
summering_omkorningar$absolut_overtakes <- abs(summering_omkorningar$netto.overtakes)
summering_omkorningar_results <- summering_omkorningar %>%
  group_by(name, year) %>%
  summarize(med_omkorningar = median(absolut_overtakes, na.rm=TRUE)) %>%
  ggplot(aes(x = factor(year), y = med_omkorningar, color = med_omkorningar)) +
  geom_boxplot(alpha = .25) + theme_classic() +
  geom_jitter(shape = 16, position = position_jitter(0.2), size = 1.5) +
  geom_smooth(method = 'loess', aes(group=1), color = 'red', lty = 2, size = 0.5, alpha = .75) +
  scale_color_gradientn(name = "", colours = rev(viridis::viridis(20))) + 
  scale_x_discrete(breaks=seq(min(summering_omkorningar$year),max(summering_omkorningar$year),10)) + 
  labs(title = "Nettoförändring i start- mot slutposition",
       subtitle = "Absoluttal borträknat för brytningar, median grupperad per race") + ylab("Median omkörningar") + xlab("Årtal") +
  guides(color = FALSE)
summering_omkorningar_results
ggsave("desk_img/omkorningar_per_ar.png", plot = summering_omkorningar_results, width = 12, height = 6, bg = "white")




##### Tidslinje över antal som kommer i mål ######
antalimal <- left_join(races[,1:2], results, by = "raceId") %>% 
  filter(year < 2022) %>%
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
  scale_y_continuous("Genomsnitt antal förare", limit = c(0, 35), sec.axis = sec_axis(~ . /0.35, name = "Procent % i mål")) +
  scale_x_continuous("Årtal", breaks = seq(1950,2021, 10)) +
  ggtitle("Genomsnittligt antal förare som startar, grupperat per race per år", subtitle = "Med genomsnittlig procent som går i mål") + 
  theme_classic() + 
  theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")
  )
ggsave("desk_img/genomsnitt_forare_i_mal.png", plot = antalimal_plot, width = 9, height = 6, bg = "white")
plot(antalimal_plot)




antalimal_2 <- left_join(races[,1:2], results, by = "raceId") %>% 
  filter(year < 2022) %>%
  group_by(raceId) %>%
  mutate(inte_med_i_race = ifelse(positionText == "E" | positionText == "F" | positionText == "N" | positionText == "W",1, 0),
         antal = n() - sum(inte_med_i_race))  %>%
          filter ( inte_med_i_race == 0) %>%
  group_by(year) %>%
  mutate(classified = ifelse(positionText == "R" | positionText == "D", 0, 1)) %>%
  summarise(medel = median(antal),
            procentimal = ( (sum(classified) / n())*100 ),
            genomsnittantalimal = medel * (procentimal/100))
          
antalimal2_plot  <- ggplot(antalimal_2, aes(year, medel)) +
  geom_col() +
  geom_line(aes(y = procentimal/3.33), color = "red", lwd = 1.5) +
  scale_y_continuous("Median antal förare", limit = c(0, 30), sec.axis = sec_axis(~ . /0.3, name = "Procent % i mål")) +
  scale_x_continuous("Årtal", breaks = seq(1950,2021, 10)) +
  ggtitle("Median antal förare som startar, grupperat per race per år", subtitle = "Med genomsnittlig procent som tar målflagg går i mål") + 
  theme_classic() + 
  theme(axis.line.y.right = element_line(color = "red"), 
        axis.ticks.y.right = element_line(color = "red"),
        axis.text.y.right = element_text(color = "red"), 
        axis.title.y.right = element_text(color = "red")
  )
ggsave("desk_img/median_forare_i_mal.png", plot = antalimal_plot, width = 9, height = 6, bg = "white")
plot(antalimal2_plot)



##### Deskriptiv statistik förare #####
drivers$age_driver <- as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(drivers$dob, "%Y"))
drivers<-left_join(drivers %>% select(-url), driversStandings,by='driverId')


results_3 <- left_join(
  results, 
  drivers %>% dplyr::rename(number_drivers = number) %>% select(-points, -position, -positionText),
  by=c('driverId','raceId')) 

results_3 <- left_join(results_3,races %>% select(-time), by='raceId')

winsDis <- results_3 %>% 
  filter(position==1) %>% 
  group_by(driverRef, circuitRef) %>% 
  summarize(count=n()) %>%
  mutate(allWins = sum(count)) %>%
  ggplot(aes(x=allWins)) +
  geom_histogram(bins=50) + theme_classic() + ggtitle("Distribution of the number of victories")
plot(winsDis)


#Barplot av de tio förarna med flest vinster, ABSOLUTTAL
winsBar_15 <- results_3 %>% 
  filter(position==1) %>%
  group_by(driverRef, country) %>% 
  summarise(count=n()) %>%
  mutate(allWins = sum(count))
top15_driver_win <- as.numeric(sort(unique(winsBar_15$allWins), decreasing = TRUE)[15])

winsBar_15_1 <- results_3 %>% 
  dplyr::filter(position==1) %>% 
  dplyr::group_by(driverRef, country) %>% 
  dplyr::summarize(count=n()) %>%
  dplyr::mutate(allWins = sum(count)) %>% 
  dplyr::filter(allWins > top15_driver_win)

winsBar_15_1_plot <- winsBar_15_1 %>%
  dplyr::group_by(driverRef, country) %>% 
  ggplot(aes(x=reorder(driverRef, allWins),y= count)) +
  geom_bar(aes(fill=country),stat='identity',color='white',size=.1) + 
  coord_flip() + theme_classic() + 
  scale_fill_manual(name="",values = viridis::viridis(33)) +
  guides(fill=guide_legend(ncol=5)) + 
  theme(legend.text= element_text(size=8),
        legend.key.size = unit(.1, "cm"),
        legend.position=c(.8,.2)) + 
  labs(title="Antal vinster per förare",
       subtitle="Topp 15") + xlab("Förare") + ylab("Antal")
plot(winsBar_15_1_plot)
ggsave("desk_img/vinster_per_forare_topp_15.png", plot = winsBar_15_1_plot, width = 12, height = 6, bg = "white")


#Barplot av de tio förarna med flest vinster, % av antal starter
results_4 <- left_join(
  results, 
  drivers %>% dplyr::rename(number_drivers = number) %>% select(-points, -position, -positionText),
  by=c('driverId','raceId')) 
results_4 <- left_join(results_4,races %>% select(-time), by='raceId')
results_4$vinst <- ifelse(results_4$position == 1, 1, 0)

winsBar_15_per <- results_4 %>% 
  group_by(driverRef) %>% 
  summarize(count = n(), vinster = sum(vinst)) %>%
  mutate(vinstprocent = vinster / count) %>%
  filter(count > 5)
top15_driver_win_per <- as.numeric(sort(unique(winsBar_15_per$vinstprocent), decreasing = TRUE)[15])

winsBar_15_per <- winsBar_15_per %>% 
  dplyr::filter(vinstprocent >= top15_driver_win_per)

winsBar_15_per_plot <- ggplot(data=winsBar_15_per, aes(x= reorder(driverRef, vinstprocent), y = vinstprocent)) +
  geom_bar(aes(fill=driverRef), stat="identity", color = 'white', size = .1) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_classic() +
  scale_fill_manual(name="",values = viridis::viridis(15)) +
  labs(title="Andel vinster per race per förare i %",
       subtitle="Topp 15") + xlab("Förare") + ylab("Vintprocent")
plot(winsBar_15_per_plot)
ggsave("desk_img/vinstprocent_per_forare_topp_15.png", plot = winsBar_15_per_plot, width = 9, height = 6, bg = "white")





##### Deskriptiv statistik konstruktör #####
constructorResults<-left_join(
  constructorResults, 
  races %>% rename(name_races = name), by='raceId')
constructorResults <- left_join(constructorResults, constructors %>% select(-url) %>% rename(name_constructor = name), by='constructorId')
constructorResults <- left_join(constructorResults, constructorStandings %>% rename(point_constructor = points) %>% select(-positionText), by=c('constructorId','raceId'))


  
winConstructors <- constructorResults %>% 
  filter(wins == 1, name_constructor %in% c('Ferrari','McLaren','Williams','Brabham','BRM', 'Tyrrell', 'Red Bull', 'Renault', 'Benetton', 'Mercedes', 'Team Lotus', 'Ligier', 'Jordan', 'Lotus F1', 'March' )) %>%
  group_by(name_constructor) %>% 
  summarize(count=n()) %>% 
  filter(count>0) %>%
  ggplot(aes(x=reorder(name_constructor, count),y= count,fill=count)) +
  geom_bar(stat='identity',color='white',size=.1) + 
  coord_flip() + theme_classic() + 
  scale_fill_gradientn(name="",colors = viridis::viridis(15)) +
  guides(fill=guide_legend(ncol=3)) + 
  theme(legend.text= element_text(size=10),
        legend.key.size = unit(.1, "cm"),
        legend.position=c(.65,.20)) + 
  labs(title="Antal vinster per Team",
       subtitle="Topp 15") + guides(fill=F) + ylab("Antal") + xlab("Konstruktör")
plot(winConstructors)
ggsave("desk_img/vinst_per_konstruktor_topp_15.png", plot = winConstructors, width = 9, height = 6, bg = "white")


#Barplot av de tio med konstruktörerna högst vinstprocent % av antal starter
winsTeam <- constructorResults %>% 
  group_by(name_constructor, year) %>% 
  summarize(count = n(), vinster = max(wins)) %>%
  mutate(vinstprocent = vinster / count) %>%
  filter(vinster > 0)

winsTeam_per <- winsTeam  %>% 
  summarize(count = sum(count), vinster = sum(vinster)) %>%
  mutate(vinstprocent = vinster / count) %>%
  filter(count > 5)
top15_driver_team_per <- as.numeric(sort(unique(winsTeam_per$vinstprocent), decreasing = TRUE)[15])


winsTeam_15_per <- winsTeam_per %>% 
  dplyr::filter(vinstprocent >= top15_driver_team_per)

winsTeam_15_per_plot <- ggplot(data=winsTeam_15_per, aes(x= reorder(name_constructor, vinstprocent), y = vinstprocent)) +
  geom_bar(aes(fill=name_constructor), stat="identity", color = 'white', size = .1) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_classic() +
  scale_fill_manual(name="",values = viridis::viridis(length(winsTeam_15_per$name_constructor))) +
  labs(title="Andel vinster per race per team i %",
       subtitle="Topp 15") + ylab("Vinstprocent") + xlab("Konsturktör")
plot(winsTeam_15_per_plot)
ggsave("desk_img/vinstprocent_per_team_topp_15.png", plot = winsTeam_15_per_plot, width = 9, height = 6, bg = "white")



##### Chordiagram på relation mellan förare och team #####

### Data ###
results <- read_csv("dat/f1db_csv/results.csv")
results$fastestLapSpeed <- as.numeric(results$fastestLapSpeed)
convertFastestLap <- function(x){
  if(length(x)>0){
    curMinute<-as.numeric(strsplit(x,":")[[1]][1])
    curSecond<-as.numeric(strsplit(strsplit(x,":")[[1]][2],"\\.")[[1]][1])
    return(curMinute*60 + curSecond)
  }
  else if(length(x)==0){
    return(NA)
  }
}
results$fastestLapTimeNum <- sapply(results$fastestLapTime, convertFastestLap)
## Races
races   <- read_csv("dat/f1db_csv/races.csv")
races$date<-as.Date(races$date,"%Y-%m-%d")
races$name<-gsub(" Grand Prix","",races$name)
results_2<-left_join(
  results %>% dplyr::select(-time, -fastestLapTime), 
  races %>% dplyr::select(-time, -url), 
  by='raceId')
## circuits
circuits   <- read_csv("dat/f1db_csv/circuits.csv")
races <- left_join(races %>% select(-name,-url), circuits %>% select(-url), by='circuitId')
## Drivers
drivers   <- read_csv("dat/f1db_csv/drivers.csv")
drivers$age_driver <- as.numeric(format(Sys.Date(), "%Y")) - as.numeric(format(drivers$dob, "%Y"))

#load driversStandings
driversStandings <- read.csv("dat/f1db_csv/driver_standings.csv")
drivers<-left_join(drivers %>% select(-url), driversStandings,by='driverId')

results_3 <- left_join(
  results, 
  drivers %>% dplyr::rename(number_drivers = number) %>% select(-points, -position, -positionText),
  by=c('driverId','raceId')) 

results_3 <- left_join(results_3,races %>% select(-time), by='raceId')

constructors <- read_csv("dat/f1db_csv/constructors.csv")
constructorStandings <- read_csv("dat/f1db_csv/constructor_standings.csv")
constructorResults<-read_csv("dat/f1db_csv/constructor_results.csv")
constructorResults<-left_join(
  constructorResults, 
  races %>% rename(name_races = name), by='raceId')
constructorResults <- left_join(constructorResults, constructors %>% select(-url) %>% rename(name_constructor = name), by='constructorId')
constructorResults <- left_join(constructorResults, constructorStandings %>% rename(point_constructor = points) %>% select(-constructorStandingsId), by=c('constructorId','raceId'))




###
results_chor <- left_join(
  results_3, 
  constructorResults %>% select(-position,-positionText,-points,-date,-country,-wins,-lng,-lat,-nationality,-circuitRef,-round, -circuitId,-year,-time,-location),
  by=c('raceId','constructorId'))
temp <- data.frame(
  results_chor %>% filter(position==1) %>% 
    group_by(name_constructor, driverRef) %>% 
    summarize(count=n()) %>% filter(count>5) %>% na.omit())

#prepare colors
names <- sort(unique(temp$name_constructor))
color <- c('#87CEEB',"gray50","gray50","#FFFFE0","gray50","#006400",'#EE0000','#1E90FF','gray50','#006400','#7F7F7F','#7F7F7F','#9C661F','#FFD700','gray50','gray50','#EEEEE0')
COL <- data.frame(name_constructor = names,color)
temp2 <- data.frame(left_join(temp, COL, by='name_constructor'))

chordDiagram(temp2[,c(1:2)], transparency = 0.5, grid.col = append(color,rep('aliceblue',17)), col= as.character(temp2$color),annotationTrack = "grid", preAllocateTracks = 1)
circos.trackPlotRegion(
  track.index = 1, 
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(
      mean(xlim), 
      ylim[1], 
      sector.name, 
      facing = "clockwise", 
      niceFacing = TRUE, 
      adj = c(0, 0.25), 
      cex=.7)
    circos.axis(
      h = "top", 
      labels.cex = 0.5, 
      major.tick.percentage = 0.2, 
      sector.index = sector.name, 
      track.index = 2)
  }, 
  bg.border = NA)



