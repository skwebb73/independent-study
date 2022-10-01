#Independent Study Research Analysis, Data Results
#For analyzing and understanding data results
#Researcher: Susie Webb

#set working directory
setwd("~/Documents/UNC/Fall 2021/Independent Study/Finished Comparisons")
library(ggplot2)
library(dplyr)
library(stringr)

webbresults <- read.csv("webbcombined.csv", stringsAsFactors = TRUE) #loaded all results

(table(webbresults$race) / nrow(webbresults)) * 100 #percentage of each race

webbresults$race <- factor(webbresults$race, #I used the factor function so I could set labels
                      levels=c(1,2,3,4,5,6), #these are three levels in the list
                      labels=c("White", "Black", "Hispanic/Latinx", "Asian/Pacific Island", "Native American", "Unidentifiable")) #I added the corresponding labels


table(webbresults$county, webbresults$race) #table of all race values


#Webb Race Results straight numbers
webbresults <- webbresults %>%
  group_by(county) 

tally <- count(webbresults, race)
tally

#Webb Race Results
aggregate(tally$n, by=list(Category=tally$county), FUN=sum) #to find total numbers

tally$sum <- c(261,261,261,261,284,284,284,284,284,243,243,243,243,364,364,364,364,364,192,
               192,192,192,192,121,121,121,121,135,135,135,135,245,245,245,245)

tally$percent <- (tally$n/tally$sum) * 100 #calcualted percents

webbraceplot <- ggplot(tally, aes(x=race, y=percent)) + 
  geom_col(width = .5) +
  facet_wrap(~ county) +
  theme_bw() + # change color scheme
  theme(axis.text.x = element_text(lineheight = .7)) +
  theme(axis.text.y = element_text(size = 5)) +
  labs(x = "Race", y = "Percent", 
       title = "Researcher 1: Racial Disparities in Twitter Feeds")+ #added axis labels and title
  coord_flip() # make horizontal

webbraceplot

#Webb Gender Results

(table(webbresults$gender) / nrow(webbresults)) * 100 #percentage of each race

webbresults$gender <- factor(webbresults$gender, #I used the factor function so I could set labels
                           levels=c(1,2,3), #these are three levels in the list
                           labels=c("Male", "Female", "Unidentifiable")) 
                           #I added the corresponding labels

table(webbresults$county, webbresults$gender) #table of all gender values

gendertally <- count(webbresults, gender)

gendertally$sum <- c(261,261,284,284,284,243,243,243,364,364,364,192,192,192,121,121,121,135,135,135,245,245,245)

gendertally$percent <- (gendertally$n/gendertally$sum) * 100 #calcualted percents

webbgenderplot <- ggplot(gendertally, aes(x=gender, y=percent)) + 
  geom_col(width = .5) +
  facet_wrap(~ county) +
  theme_bw() + # change color scheme
  theme(axis.text.x = element_text(lineheight = .7)) +
  theme(axis.text.y = element_text(size = 5)) +
  labs(x = "Gender", y = "Percent", 
       title = "Researcher 1: Gender Disparities in Twitter Feeds")+ #added axis labels and title
  coord_flip() # make horizontal

webbgenderplot

#Webb race without missing persons

webbresults$missing <- str_detect(webbresults$text, "[mM][iI][sS][sS][iI][nN][gG]|[sS][iI][lL][vV][eE][rR]|[rR][uU][nN][aA][wW][aA][yY]")
#codes for missing subjects

missingdata <- subset(webbresults, type == 3)

nomissingwebbresults <- subset(webbresults, type != 3)

missingdata2 <-subset(nomissingwebbresults, missing = TRUE)
table(missingdata2$missing)

nomissingwebbresults <- subset(nomissingwebbresults, missing != TRUE)

table(nomissingwebbresults$type)
table(nomissingwebbresults$missing)

nomissingwebbresults <- nomissingwebbresults %>%
  group_by(county) 

tally <- count(nomissingwebbresults, race)
tally

aggregate(tally$n, by=list(Category=tally$county), FUN=sum) #to find total numbers

tally$sum <- c(260,260,260,260,174,174,174,174,174,192,192,192,192,281,281,281,281,281,
               175,175,175,175,175,114,114,114,114,106,106,106,106,170,170,170,170)

tally$percent <- (tally$n/tally$sum) * 100 #calcualted percents


webbnomissingraceplot <- ggplot(tally, aes(x=race, y=percent)) + 
  geom_col(width = .5) +
  facet_wrap(~ county) +
  theme_bw() + # change color scheme
  theme(axis.text.x = element_text(lineheight = .7)) +
  theme(axis.text.y = element_text(size = 5)) +
  labs(x = "Race", y = "Percent", 
       title = "Researcher 1: Missing Omitted, Racial Disparities")+ #added axis labels and title
  coord_flip() # make horizontal

webbnomissingraceplot


#Now onto Wilson results

#Wilson RAce

wilsonresults <- read.csv("wilsoncombined.csv", stringsAsFactors = TRUE) #loaded all results

(table(wilsonresults$race) / nrow(wilsonresults)) * 100 #percentage of each race

wilsonresults$race <- factor(wilsonresults$race, #I used the factor function so I could set labels
                           levels=c(1,2,3,4,5,6), #these are three levels in the list
                           labels=c("White", "Black", "Hispanic/Latinx", "Asian/Pacific Island", "Native American", "Unidentifiable")) #I added the corresponding labels


table(wilsonresults$county, wilsonresults$race) #table of all race values

#Wilson Race Results straight numbers
wilsonresults <- wilsonresults %>%
  group_by(county) 

tally <- count(wilsonresults, race)
tally

#Wilson Race Results
aggregate(tally$n, by=list(Category=tally$county), FUN=sum) #to find total numbers

table(wilsonresults$county)

tally$sum <- c(261,261,261,261,261,265,265,265,265,237,237,237,237,237,358,358,358,358,358,
               193,193,193,193,121,121,121,121,121,135,135,135,135,135,380,380,380,380,380)

tally$percent <- (tally$n/tally$sum) * 100 #calcualted percents

wilsonraceplot <- ggplot(tally, aes(x=race, y=percent)) + 
  geom_col(width = .5) +
  facet_wrap(~ county) +
  theme_bw() + # change color scheme
  theme(axis.text.x = element_text(lineheight = .7)) +
  theme(axis.text.y = element_text(size = 5)) +
  labs(x = "Race", y = "Percent", 
       title = "Researcher 2: Racial Disparities in Twitter Feeds")+ #added axis labels and title
  coord_flip() # make horizontal

wilsonraceplot

#Wilson Gender Results
(table(wilsonresults$gender) / nrow(wilsonresults)) * 100 #percentage of each race

wilsonresults$gender <- factor(wilsonresults$gender, #I used the factor function so I could set labels
                             levels=c(1,2,3), #these are three levels in the list
                             labels=c("Male", "Female", "Unidentifiable")) 
#I added the corresponding labels

table(wilsonresults$county, wilsonresults$gender) #table of all gender values

gendertally <- count(wilsonresults, gender)

gendertally$sum <- c(261,261,265,265,265,237,237,237,358,358,358,193,193,121,121,121,135,135,135,380,380)

gendertally$percent <- (gendertally$n/gendertally$sum) * 100 #calcualted percents

wilsongenderplot <- ggplot(gendertally, aes(x=gender, y=percent)) + 
  geom_col(width = .5) +
  facet_wrap(~ county) +
  theme_bw() + # change color scheme
  theme(axis.text.x = element_text(lineheight = .7)) +
  theme(axis.text.y = element_text(size = 5)) +
  labs(x = "Gender", y = "Percent", 
       title = "Researcher 2: Gender Disparities in Twitter Feeds")+ #added axis labels and title
  coord_flip() # make horizontal

wilsongenderplot

#Wilson race without missing persons

wilsonresults$missing <- str_detect(wilsonresults$text, "[mM][iI][sS][sS][iI][nN][gG]|[sS][iI][lL][vV][eE][rR]|[rR][uU][nN][aA][wW][aA][yY]")
#codes for missing subjects

table(wilsonresults$type)
table(webbresults$type)

missingdata <- subset(wilsonresults, type == 3)

nomissingwilsonresults <- subset(wilsonresults, type != 3)

missingdata2 <-subset(nomissingwilsonresults, missing = TRUE)
table(missingdata2$missing)

nomissingwilsonresults <- subset(nomissingwilsonresults, missing != TRUE)

table(nomissingwilsonresults$type)
table(nomissingwilsonresults$missing)

nomissingwilsonresults <- nomissingwilsonresults %>%
  group_by(county) 

tally <- count(nomissingwilsonresults, race)
tally

aggregate(tally$n, by=list(Category=tally$county), FUN=sum) #to find total numbers

tally$sum <- c(251,251,251,251,251,171,171,171,171,185,185,185,185,185,271,271,271,
               271,172,172,172,172,114,114,114,114,114,111,111,111,111,260,260,260,260,260)

tally$percent <- (tally$n/tally$sum) * 100 #calcualted percents


wilsonnomissingraceplot <- ggplot(tally, aes(x=race, y=percent)) + 
  geom_col(width = .5) +
  facet_wrap(~ county) +
  theme_bw() + # change color scheme
  theme(axis.text.x = element_text(lineheight = .7)) +
  theme(axis.text.y = element_text(size = 5)) +
  labs(x = "Race", y = "Percent", 
       title = "Researcher 2: Missing Omitted, Racial Disparities")+ #added axis labels and title
  coord_flip() # make horizontal

wilsonnomissingraceplot


#combined results of both webb and wilson

finalresults <- read.csv("finalcodes.csv", stringsAsFactors = TRUE) #loaded all results

(table(finalresults$race) / nrow(finalresults)) * 100 #percentage of each race

finalresults$race <- factor(finalresults$race, #I used the factor function so I could set labels
                           levels=c(1,2,3,4,5,6), #these are three levels in the list
                           labels=c("White", "Black", "Hispanic/Latinx", "Asian/Pacific Island", "Native American", "Unidentifiable")) #I added the corresponding labels


table(finalresults$county, finalresults$race) #table of all race values

#Final Race Results straight numbers
finalresults <- finalresults %>%
  group_by(county) 

tally <- count(finalresults, race)
tally

aggregate(tally$n, by=list(Category=tally$county), FUN=sum) #to find total numbers

tally$sum <- c(522,522,522,522,522,549,549,549,549,549,480,480,480,480,480,722,722,722,
               722,722,722,385,385,385,385,385,242,242,242,242,242,270,270,270,270,270,625,625,625,625,625)

tally$percent <- (tally$n/tally$sum) * 100 #calcualted percents

finalraceplot <- ggplot(tally, aes(x=race, y=percent)) + 
  geom_col(width = .5) +
  facet_wrap(~ county) +
  theme_bw() + # change color scheme
  theme(axis.text.x = element_text(lineheight = .7)) +
  theme(axis.text.y = element_text(size = 5)) +
  labs(x = "Race", y = "Percent", 
       title = "Racial Disparities in Twitter Feeds")+ #added axis labels and title
  coord_flip() # make horizontal

finalraceplot

#Combined Gender Results
(table(finalresults$gender) / nrow(finalresults)) * 100 #percentage of each race

finalresults$gender <- factor(finalresults$gender, #I used the factor function so I could set labels
                             levels=c(1,2,3), #these are three levels in the list
                             labels=c("Male", "Female", "Unidentifiable")) 
#I added the corresponding labels

table(finalresults$county, finalresults$gender) #table of all gender values

gendertally <- count(finalresults, gender)

gendertally$sum <- c(522,522,549,549,549,480,480,480,722,722,722,385,385,385,242,242,242,270,270,270,625,625,625)

gendertally$percent <- (gendertally$n/gendertally$sum) * 100 #calcualted percents

finalgenderplot <- ggplot(gendertally, aes(x=gender, y=percent)) + 
  geom_col(width = .5) +
  facet_wrap(~ county) +
  theme_bw() + # change color scheme
  theme(axis.text.x = element_text(lineheight = .7)) +
  theme(axis.text.y = element_text(size = 5)) +
  labs(x = "Gender", y = "Percent", 
       title = "Gender Disparities in Twitter Feeds")+ #added axis labels and title
  coord_flip() # make horizontal

finalgenderplot

#Final combined race without missing persons

finalresults$missing <- str_detect(finalresults$text, "[mM][iI][sS][sS][iI][nN][gG]|[sS][iI][lL][vV][eE][rR]|[rR][uU][nN][aA][wW][aA][yY]")
#codes for missing subjects

missingdata <- subset(finalresults, type == 3)

nomissingfinalresults <- subset(finalresults, type != 3)

missingdata2 <-subset(nomissingfinalresults, missing = TRUE)
table(missingdata2$missing)

nomissingfinalresults <- subset(nomissingfinalresults, missing != TRUE)

table(nomissingfinalresults$type)
table(nomissingfinalresults$missing)

nomissingfinalresults <- nomissingfinalresults %>%
  group_by(county) 

tally <- count(nomissingfinalresults, race)
tally

aggregate(tally$n, by=list(Category=tally$county), FUN=sum) #to find total numbers

tally$sum <- c(511,511,511,511,511,345,345,345,345,345,377,377,377,377,377,552,552,552,
               552,552,347,347,347,347,347,228,228,228,228,228,217,217,217,217,430,430,430,430,430)

tally$percent <- (tally$n/tally$sum) * 100 #calcualted percents


finalnomissingraceplot <- ggplot(tally, aes(x=race, y=percent)) + 
  geom_col(width = .5) +
  facet_wrap(~ county) +
  theme_bw() + # change color scheme
  theme(axis.text.x = element_text(lineheight = .7)) +
  theme(axis.text.y = element_text(size = 5)) +
  labs(x = "Race", y = "Percent", 
       title = "Missing Omitted, Racial Disparities in Twitter Feeds")+ #added axis labels and title
  coord_flip() # make horizontal

finalnomissingraceplot

#other analysis

#confidencecodes
table(webbresults$race_confidence)
table(wilsonresults$race_confidence)

#types
table(webbresults$type)
table(wilsonresults$type)

