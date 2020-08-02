#Adapted from Dr.Follis
# ---------------------------------------------------------
# Step 1: Import Library
# ---------------------------------------------------------
library(xml2)
library(rvest)
library(tidyverse)

#We are going to work with TV Data by season
#http://www.tv.com/shows/the-simpsons/season-1/

# ---------------------------------------------------------
# Step 2: Structure & Scraping Data
# ---------------------------------------------------------
xxx <- address %>% read_html() %>% html_nodes("._rating") %>% html_text()
xxx <- address %>% read_html() %>% html_nodes(".title") %>% html_text()
xxx <- address %>% read_html() %>% html_nodes(".ep_info") %>% html_text()
xxx <- address %>% read_html() %>% html_nodes(".description") %>% html_text()
# ---------------------------------------------------------
# Step 3: Structure & Scrape Data + ForLoop the Season
# ---------------------------------------------------------
simpsons.tv.com <- data.frame()
for (i in 1:30) {
  address=paste0("http://www.tv.com/shows/the-simpsons/season-",i,"/")
  tv.com.rating <- address %>% 
    read_html() %>% 
    html_nodes("._rating") %>% 
    html_text() %>% 
    as.numeric() %>% 
    as.data.frame()
  num.epi <- nrow(tv.com.rating)
  episode.name <- address %>% 
    read_html() %>% 
    html_nodes(".title") %>% 
    html_text() %>% 
    as.data.frame()
  episode.name <- episode.name[1:num.epi,]
  description <- address %>% 
    read_html() %>% 
    html_nodes(".description") %>% 
    html_text()
  xxx <- data.frame(tv.com.rating=tv.com.rating,episode.name=episode.name,description=description,season=i)
  simpsons.tv.com <- rbind(simpsons.tv.com,xxx)
}
# ---------------------------------------------------------
# Step 4: Naming
# ---------------------------------------------------------
names(simpsons.tv.com) <- c("tv.com.rating","episode.name","description","season")
# ---------------------------------------------------------
# Step 5: View Data 
# ---------------------------------------------------------
View(simpsons.tv.com)
