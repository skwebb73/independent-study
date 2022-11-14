#Independent Study Cleaned R Code
#For scraping Twitter and Downloading the Images
#Researcher: Susie Webb

setwd("~/Documents/UNC/Fall 2021/Independent Study")

library(httr)
library(jsonlite)
library(dplyr)
library(rtweet)
library(stringr)


consumer_key = ""
consumer_secret = ""
access_token = ""
access_secret = ""

token <- create_token(
  app = "independent_study",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret)

policedept <- get_timelines(c("policedept"), n = 4000)

policedept$missing <- str_detect(policedept$text, "[mM][iI][sS][sS][iI][nN][gG]|[sS][iI][lL][vV][eE][rR]|[rR][uU][nN][aA][wW][aA][yY]")

policedept$media_type <- unlist(policedept$media_type)
policedept$media_url <- unlist(policedept$media_url)

table(policedept$media_type)

write.csv(policedept[,c("status_id", "created_at", "screen_name", "text", "favorite_count",
                   "retweet_count", "media_type", "missing", "media_url")], "policedept.csv", na = "NA", row.names=FALSE)

#to download the images
policedept <- read.csv("policedept.csv")

media_url <- policedept[!is.na(policedept$media_url),]

for(i in media_url$media_url){
  fn = str_remove_all(i, "[^A-Za-z0-9_.\\-]")
  download.file(i, fn, mode="wb")
}


