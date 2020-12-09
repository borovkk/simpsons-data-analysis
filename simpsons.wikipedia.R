# Inspired by JAKE DANIELS
#https://datacritics.com/2018/08/15/build-a-ggplot-the-fall-of-the-simpsons/?utm_campaign=News&utm_medium=Community&utm_source=DataCamp.com
#Adapted from Dr.Follis
# ---------------------------------------------------------
# Step 1: Import Library
# ---------------------------------------------------------
library(xml2)
library(rvest)
library(tidyverse)
# ---------------------------------------------------------
# Step 2: Reading URL & Data Scraping
# ---------------------------------------------------------
s.url <- "https://en.wikipedia.org/wiki/The_Simpsons"

s.url <-"https://en.wikipedia.org/wiki/List_of_The_Simpsons_episodes_(seasons_1%E2%80%9320)#Episodes"

# ---------------------------------------------------------
# Step 3: Structure & Scrape Data + ForLoop 
# ---------------------------------------------------------
sim.ep <- data.frame()
for (i in 2:19) {
  sim.ex <- s.url %>%
    read_html() %>%
    html_node(xpath = paste('//*[@id="mw-content-text"]/div/table[',i,']',sep="")) %>%
    html_table(fill = TRUE)
  sim.ex$season=i-1
  sim.ep <- rbind(sim.ep,sim.ex)
}

for (i in 21:22) {
  sim.ex <- s.url %>%
    read_html() %>%
    html_node(xpath = paste('//*[@id="mw-content-text"]/div/table[',i,']',sep="")) %>%
    html_table(fill = TRUE)
  sim.ex$season=i-2
  sim.ep <- rbind(sim.ep,sim.ex)
}
s.url <-"https://en.wikipedia.org/wiki/List_of_The_Simpsons_episodes"
for (i in 3:5) {
  sim.ex <- s.url %>%
    read_html() %>%
    html_node(xpath = paste('//*[@id="mw-content-text"]/div/table[',i,']',sep="")) %>%
    html_table(fill = TRUE)
  sim.ex$season=i+18
  sim.ep <- rbind(sim.ep,sim.ex)
}

for (i in 7:13) {
  sim.ex <- s.url %>%
    read_html() %>%
    html_node(xpath = paste('//*[@id="mw-content-text"]/div/table[',i,']',sep="")) %>%
    html_table(fill = TRUE)
  sim.ex$season=i+17
  sim.ep <- rbind(sim.ep,sim.ex)
}
# ---------------------------------------------------------
# Step 4: Naming
# ---------------------------------------------------------
names(sim.ep)=c("episode.no","inseason.no","title","director","writer","original.air.date","prod.code","US.viewers","season")
sim.ep$US.viewers <- gsub("\\[+[0-9]*]","",sim.ep$US.viewers)
sim.ep$US.viewers <- as.numeric(sim.ep$US.viewers)
sim.ep$original.air.date <- gsub("\\s\\(+[0-9]*\\-+[0-9]*\\-+[0-9]*\\)","",sim.ep$original.air.date)
sim.ep$title <- gsub("\\\"","",sim.ep$title)

# ---------------------------------------------------------
# Step 5: View Data 
# ---------------------------------------------------------
View(sim.ep)






