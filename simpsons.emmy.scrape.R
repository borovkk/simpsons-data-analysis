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
url <- 'https://www.imdb.com/title/tt0096697/awards'

xxx <- url %>% read_html() %>% html_nodes('.title_award_outcome') %>% html_text()
xxx <- as.data.frame(xxx)
View(xxx)
xxx2 <- url %>% read_html() %>% html_nodes('#main :nth-child(1)') %>% html_text()
xxx2 <- as.data.frame(xxx2)
View(xxx2)

xxx3 <- strsplit(as.character(xxx2[,1]),"\n")
View(xxx3)
xxx3.1 <- unlist(xxx3[[1]])
View(xxx3.1)
# ---------------------------------------------------------
# Step 3: Data Scraping Pattern Matching And Replacement
# ---------------------------------------------------------
grep("Emmy Awards",xxx3.1)
grep("[0-9]+",xxx3.1)
grep("WinnerPrime",xxx3.1)
grep("NomineePrime",xxx3.1)
grep("BAFTA",xxx3.1)  #cut at BAFTA

xxx3.1 <- xxx3.1[29:2084]
xxx3.2 <- data.frame(Year=xxx3.1[grep("Emmy Awards",xxx3.1)+1],
                     x1=grep("Emmy Awards",xxx3.1),
                     x2=c(grep("Emmy Awards",xxx3.1)[2:length(grep("Emmy Awards",xxx3.1))],2054),
                     Emmy_win=NA)
# --------------------------------------------------------------
# Step 4: Structure & Scrape Data + ForLoop Emmy Winner Y or N
# --------------------------------------------------------------
for(i in 1:nrow(xxx3.2)) {
  xxx3.2$Emmy_win[i]=ifelse(length(which(grep("WinnerPrime",xxx3.1)>xxx3.2$x1[i] & grep("WinnerPrime",xxx3.1)<xxx3.2$x2[i])>0)==1,"Y","N")
}

xxx3.2 <- xxx3.2 %>% select(Year,Emmy_win)
simp.emmy <- xxx3.2
View(simp.emmy)



