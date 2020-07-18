library(tidyverse)
library(ggplot2)
library(grid)
library(dplyr)


names(all_simpsons)
str(all_simpsons)
head(all_simpsons)
glimpse(all_simpsons)
summary(all_simpsons)
view(all_simpsons)

#prep the data by avg US Viewers 
viewers.all <- all_simpsons %>%
  group_by(season.wiki) %>%
  summarize(avg.view=mean(US.viewers,na.rm=T)) %>%
  right_join(all_simpsons) # get the rest of the data

############################
# Visual 1 Viewers By Season 
############################
ggplot(viewers.all,aes(x=as.factor(season.wiki),y=US.viewers,fill=avg.view))+
  geom_bar(stat='identity', fill="dark grey")+
  ggtitle("US Viewers by Season")+
  labs(title="Simpsons Popularity",
      subtitle = "Split by Season", 
      caption = "Made by Group 1 \n Source: IMDB, Wikipedia \n Inspired by Jake Daniels")+
  xlab("Season Number")+
  ylab("Veiwers in M")+
  theme(plot.title = element_text(hjust=.5,size=25),
        plot.subtitle = element_text(hjust=.5,size=18))+
  annotate("rect", xmin=1.5, xmax=9, ymin=257, ymax=536.9, alpha = .2, fill = "green") +
  annotate("text", x=5.5, y=450, label = "Simpsons Mania", color = "black") +
  # highlighting: Zombie Simpsons
  annotate("rect", xmin=17.5, xmax=30.5, ymin=50, ymax=200, alpha = .2, fill = "red") +
  annotate("text", x=25, y= 160, label = "Zombie Simpsons", color = "black")

####################
#Visual 2 by ratings
####################

sim <- all_simpsons %>%
  select(season.wiki,episode.no,original.air.date,US.viewers,Rating_IMDB,tv.com.rating,Episode,description,director,writer)
summary(sim)
sim %>%
  group_by(season.wiki) %>%
  #summarize both ratings columns: IMDB & TV.COM
  summarize(avg.IMDBrating=mean(Rating_IMDB,na.rm=T),
            avg.TV.COMrating=mean(tv.com.rating,na.rm=T))%>%
  ggplot(aes(season.wiki)) +
  geom_line(aes(y = avg.IMDBrating,col="IMDB")) +
  geom_point(aes(y = avg.IMDBrating,col="IMDB")) +
  annotate(geom="rect", xmin=20, xmax=30, ymin=5, ymax=8.5, alpha = .2, fill = "red") +
  annotate(geom="text", x=25, y=7, label = "Ratings Decline", color = "black")+
  geom_line(aes(y = avg.TV.COMrating,col="TV.COM"))+
  geom_point(aes(y = avg.TV.COMrating,col="TV.COM"))+
  coord_cartesian(ylim=c(4,10)) + 
  scale_y_continuous(breaks=seq(0, 10, 2))+
  xlim(2,30)+
  labs(y="Ratings",
       x="Season",
       title="Decline of the Simpsons",
       subtitle="Ratings by Season", 
       caption = "Made by Group 1 \n Source: IMDB, Wikipedia, TV.com")+
  theme(plot.title = element_text(hjust=.5,size=25),
        plot.subtitle = element_text(hjust=.5,size=18),
        panel.background = element_rect(),
        plot.background = element_rect(fill="white"),
        legend.background = element_rect(size=.5, linetype="dotted", colour = "dark grey"),
        legend.justification=c(0,-0), legend.position=c(0,0),
        legend.title=element_blank())

################################################
#directors and writors vizs
################################################

# create a data frame using only the columns "director" and "Rating_IMDB"
df <- data.frame("Director" = all_simpsons$director, "IMDB.Rating" = all_simpsons$Rating_IMDB)
# include only top 10 most frequent directors
names = c("Mark Kirkland", "Steven Dean Moore", "Bob Anderson", "Matthew Nastuk", "Jim Reardon", "Michael Polcino", "Nancy Kruse", "Chris Clements", "Lance Kramer", "Mike B. Anderson")
df <- subset(df, Director %in% names)
# remove rows containing NA in "ratings"
df <- df %>% filter(!is.na(IMDB.Rating))
# get the average for each director
average = c(
  sum(df[which(df[,1] == names[1]), 2]) / sum(df[,1] == names[1]),
  sum(df[which(df[,1] == names[2]), 2]) / sum(df[,1] == names[2]),
  sum(df[which(df[,1] == names[3]), 2]) / sum(df[,1] == names[3]),
  sum(df[which(df[,1] == names[4]), 2]) / sum(df[,1] == names[4]),
  sum(df[which(df[,1] == names[5]), 2]) / sum(df[,1] == names[5]),
  sum(df[which(df[,1] == names[6]), 2]) / sum(df[,1] == names[6]),
  sum(df[which(df[,1] == names[7]), 2]) / sum(df[,1] == names[7]),
  sum(df[which(df[,1] == names[8]), 2]) / sum(df[,1] == names[8]),
  sum(df[which(df[,1] == names[9]), 2]) / sum(df[,1] == names[9]),
  sum(df[which(df[,1] == names[10]), 2] / sum(df[,1] == names[10])))

# create a new data frame with the columns "director" and "average rating"
newDf <- data.frame("Director" = names, "Average_Rating" = average)
# make a barplot out of the variables "column" and "average rating"
ggplot(newDf, aes(fill = Director, 
                  y = Average_Rating, x = Director)) + 
  geom_bar(stat = "identity")+
  labs(title="Director",
       subtitle = "Average Rating", 
       caption = "Made by Group 1 \n Source: IMDB, Wikipedia, tv.com")+
  xlab("Directors")+
  ylab("Average Rating")+
  theme(plot.title = element_text(hjust=.5,size=25),
        plot.subtitle = element_text(hjust=.5,size=18))
#######
#create a data frame using only the columns "director" and "season"
df <- data.frame("Director" = all_simpsons$director, "Season" = all_simpsons$season.wiki)
# include only top 10 most frequent directors
names = c("Mark Kirkland", "Steven Dean Moore", "Bob Anderson", "Matthew Nastuk", "Jim Reardon", "Michael Polcino", "Nancy Kruse", "Chris Clements", "Lance Kramer", "Mike B. Anderson")
df <- subset(df, Director %in% names)
# add count "n" to the data frame for each unique combination of director and season
df <- df %>% count(Director, Season)
# rename column "n" to "episodes directed"
df <- df %>% rename(Episodes_Directed = n)
#################################
#### Main code for the Viz 3 ####
#################################
# make a stacked barchart out of the categorical variables "column" and "season" with count in the y-axis
ggplot(df, aes(fill = Director, y = Episodes_Directed, x = Season))+
  geom_bar(position = "stack", stat = "identity")+
  labs(title="Simpsons by Average Rating",
       subtitle = "with episodes split by top 10 Directors", 
       caption = "Made by Group 1 \n Source: IMDB, Wikipedia, tv.com")+
  xlab("Seasons")+
  ylab("Episodes split by Top 10 Directors")+
  scale_fill_brewer(palette = "Paired") + 
  theme(plot.title = element_text(hjust=.5,size=25),
        plot.subtitle = element_text(hjust=.5,size=18))
########
#By Writers
########

# create a data frame using only the columns "writer" and "season"
df.writers <- data.frame("Writer" = all_simpsons$writer, "Season" = all_simpsons$season.wiki)
# sort(newTable, decreasing = T)
# include only top 10 most frequent writers
names = c(
  "John Swartzwelder",
  "Joel H. Cohen",
  "Matt Selman",
  "Tim Long",
  "Michael Price",
  "J. Stewart Burns",
  "John Frink",
  "Jon Vitti",
  "Jeff Westbrook",
  "Carolyn Omine")
df.writers <- subset(df.writers, Writer %in% names)
# add count "n" to the data frame for each unique combination of writer and season
df.writers <- df.writers %>% count(Writer, Season)
# rename column "n" to "episodes written"
df.writers <- df.writers %>% rename(Episodes_Written = n)
# make a stacked barchart out of the categorical variables "writer" and "season" with count in the y-axis
#################################
#### Main code for the Viz 4 ####
################################# 
ggplot(df.writers, aes(fill = Writer, y = Episodes_Written, x = Season)) + 
  geom_bar(position = "stack", stat = "identity")+
  labs(title="Simpsons by Average Rating",
       subtitle = "with episodes split by top 10 Writers", 
       caption = "Made by Group 1 \n Source: IMDB, Wikipedia, tv.com")+
  xlab("Seasons")+
  ylab("Episodes split by Top 10 Writers")+
  scale_fill_brewer(palette = "Paired") + 
  theme(plot.title = element_text(hjust=.5,size=25),
        plot.subtitle = element_text(hjust=.5,size=18))

