# Inspired by JAKE DANIELS
#https://datacritics.com/2018/08/15/build-a-ggplot-the-fall-of-the-simpsons/?utm_campaign=News&utm_medium=Community&utm_source=DataCamp.com
#Adapted from Dr.Follis

#---------------------------------------------------------
# Step 1: Import Library
# ---------------------------------------------------------
library(xml2)
library(rvest)
library(tidyverse)

# ---------------------------------------------------------
# Step 2: Reading URL & Data Scraping
# ---------------------------------------------------------
page <- 'https://www.imdb.com/title/tt0096697/episodes?season=1'

xxx <- url %>% read_html() %>% html_nodes('#episodes_content strong a') %>% html_text()
xxx <- as.data.frame(xxx)
View(xxx)
xxx <- url %>% read_html() %>% html_nodes('.ipl-rating-star__rating') %>% html_text()
xxx <- as.data.frame(xxx)
View(xxx)
# ---------------------------------------------------------
# Step 3: Structure & Scrape Data + ForLoop the Season
# ---------------------------------------------------------
simp.imdb=data.frame()
for (i in 1:30) {
  page = paste0('https://www.imdb.com/title/tt0096697/episodes?season=',i)
  xxx <- data.frame(
    name <- page %>% read_html() %>% html_nodes('#episodes_content strong a') %>% html_text() %>% as.data.frame(),
    rating <- page %>% read_html() %>% html_nodes('.ipl-rating-widget > .ipl-rating-star .ipl-rating-star__rating') %>% html_text() %>% as.data.frame(),
    details <- page %>% read_html() %>% html_nodes('.zero-z-index div') %>% html_text() %>% as.data.frame(),
    season <- i
  )
  xxx$season.episode.no <- seq(1:nrow(xxx))
  simp.imdb = rbind(simp.imdb,xxx)
}
# ---------------------------------------------------------
# Step 4: Naming
# ---------------------------------------------------------
names(simp.imdb) <- c("Episode","Rating","Season_Episode","Season","Season.Episode.No")
# ---------------------------------------------------------
# Step 5: View Data 
# ---------------------------------------------------------

