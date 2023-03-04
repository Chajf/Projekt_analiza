library(tidyverse)
library(rvest)

LEC <- NULL
LEC <- as.data.frame(LEC)
for(i in c(1460:1468,1719)){
  url <- paste0("https://gol.gg/teams/team-matchlist/",i,"/split-ALL/tournament-ALL/")
  page <- read_html(url)
  
  x <- page %>% 
    html_nodes(xpath="/html/body/div/main/div[2]/div/div[3]/div/div/div/table") %>% 
    html_table()
  
  name <- page %>% 
    html_nodes(xpath="//h1") %>% 
    html_text()
  
  x <- as.data.frame(x) %>% 
    .[,-c(3,14,16)]
  
  colnames(x) <- c("Result","Score","Kills","Golds","Towers","Dragons","Vs","Vs_kills","Vs_golds",
                     "Vs_towers","Vs_dragons","Duration","Patch","Tournament")
  x$Team <- name
  x$Region <- "LEC"
  
  LEC <- rbind(LEC,x)
}
LEC$Patch <- as.factor(LEC$Patch)
LEC$Tournament <- as.factor(LEC$Tournament)
LEC <- LEC %>% 
  mutate(Duration_sec=as.numeric(substr(Duration,1,2))*60+as.numeric(substr(Duration,4,5)))
LEC <- LEC %>% 
  mutate(Win=(Result=="WIN"))


LPL <- NULL
LPL <- as.data.frame(LPL)
for(i in 1421:1437){
  url <- paste0("https://gol.gg/teams/team-matchlist/",i,"/split-ALL/tournament-ALL/")
  page <- read_html(url)
  
  x <- page %>% 
    html_nodes(xpath="/html/body/div/main/div[2]/div/div[3]/div/div/div/table") %>% 
    html_table()
  
  name <- page %>% 
    html_nodes(xpath="//h1") %>% 
    html_text()
  
  x <- as.data.frame(x) %>% 
    .[,-c(3,14,16)]
  
  colnames(x) <- c("Result","Score","Kills","Golds","Towers","Dragons","Vs","Vs_kills","Vs_golds",
                   "Vs_towers","Vs_dragons","Duration","Patch","Tournament")
  x$Team <- name
  x$Region <- "LPL"
  
  LPL <- rbind(LPL,x)
}
LPL$Patch <- as.factor(LPL$Patch)
LPL$Tournament <- as.factor(LPL$Tournament)
LPL <- LPL %>% 
  mutate(Duration_sec=as.numeric(substr(Duration,1,2))*60+as.numeric(substr(Duration,4,5)))
LPL <- LPL %>% 
  mutate(Win=(Result=="WIN"))

LCK <- NULL
LCK <- as.data.frame(LCK)
for(i in c(1411,1412:1414,1416:1419,1438:1439)){
  url <- paste0("https://gol.gg/teams/team-matchlist/",i,"/split-ALL/tournament-ALL/")
  page <- read_html(url)
  
  x <- page %>% 
    html_nodes(xpath="/html/body/div/main/div[2]/div/div[3]/div/div/div/table") %>% 
    html_table()
  
  name <- page %>% 
    html_nodes(xpath="//h1") %>% 
    html_text()
  
  x <- as.data.frame(x) %>% 
    .[,-c(3,14,16)]
  
  colnames(x) <- c("Result","Score","Kills","Golds","Towers","Dragons","Vs","Vs_kills","Vs_golds",
                   "Vs_towers","Vs_dragons","Duration","Patch","Tournament")
  x$Team <- name
  x$Region <- "LCK"
  
  LCK <- rbind(LCK,x)
}
LCK$Patch <- as.factor(LCK$Patch)
LCK$Tournament <- as.factor(LCK$Tournament)
LCK <- LCK %>% 
  mutate(Duration_sec=as.numeric(substr(Duration,1,2))*60+as.numeric(substr(Duration,4,5)))
LCK <- LCK %>% 
  mutate(Win=(Result=="WIN"))

LCS <- NULL
LCS <- as.data.frame(LCS)
for(i in c(1440:1449)){
  url <- paste0("https://gol.gg/teams/team-matchlist/",i,"/split-ALL/tournament-ALL/")
  page <- read_html(url)
  
  x <- page %>% 
    html_nodes(xpath="/html/body/div/main/div[2]/div/div[3]/div/div/div/table") %>% 
    html_table()
  
  name <- page %>% 
    html_nodes(xpath="//h1") %>% 
    html_text()
  
  x <- as.data.frame(x) %>% 
    .[,-c(3,14,16)]
  
  colnames(x) <- c("Result","Score","Kills","Golds","Towers","Dragons","Vs","Vs_kills","Vs_golds",
                   "Vs_towers","Vs_dragons","Duration","Patch","Tournament")
  x$Team <- name
  x$Region <- "LCS"
  
  LCS <- rbind(LCS,x)
}
LCS$Patch <- as.factor(LCS$Patch)
LCS$Tournament <- as.factor(LCS$Tournament)
LCS <- LCS %>% 
  mutate(Duration_sec=as.numeric(substr(Duration,1,2))*60+as.numeric(substr(Duration,4,5)))
LCS <- LCS %>% 
  mutate(Win=(Result=="WIN"))

LOL <- rbind(LEC,LCS,LPL,LCK)

save(LOL,LCK,LCS,LEC,LPL,file="C:\\Users\\Patryk\\Desktop\\Studia\\Projekt_analiza\\lol_data.rda")
load(file="C:\\Users\\Patryk\\Desktop\\Studia\\Projekt_analiza\\lol_data.rda")
