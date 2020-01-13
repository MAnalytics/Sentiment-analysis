#source:
#https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/
#https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/sentiment-analysis-of-twitter-data-r/  #for sentiment analysis
#get sentiment dictionary/lexicon https://www.tidytextmining.com/sentiment.html#the-sentiments-dataset 
#https://brexit.foraction.gr/

setwd("C:/Users/monsu/Desktop/software paper/")

## load rtweet
install.packages("rtweet")
library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)

#post_tweet("Trying tweeting from within R! ")
## your tweet has been posted!


install.packages("twitteR")
install.packages("ROAuth")
install.packages("syuzhet")
install.packages("tm")
install.packages("SnowballC")
install.packages("stringi")
install.packages("topicmodels")
install.packages("syuzhet")
library(twitteR)
library(ROAuth)
library(NLP)#, lib.loc="~/R/win-library/3.3")
library(twitteR)#, lib.loc="~/R/win-library/3.3")
library(syuzhet)#, lib.loc="~/R/win-library/3.3")
library(tm)#, lib.loc="~/R/win-library/3.3")
library(SnowballC)#", lib.loc="~/R/win-library/3.3")
library(stringi)#", lib.loc="~/R/win-library/3.3")
library(topicmodels)
library(lubridate)

#setting up twitter
consumer_key <- 'rJWorDnMoARYE7OUTqaz0rOo4'
consumer_secret <- 'dUJktwlOwdbaUNB15z2Yw4HI3piOd1aTevADmathrNhBdLC3ny'
access_token <- '1108852279434715136-4IpdsZ2t69wcj0GZ3ricwL8XsU0RzT'
access_secret <- 'eX78KEUNtaU6GZ0wFay5lafhbzeZhuLzHkq71RxKnt9xj'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#function to pick random row in a dataframe
pickRandomRows = function(df, numberOfRows = 10) {
df %>% dplyr::slice(sample(1:length(df[,1]), numberOfRows, replace=FALSE))
}

#function to put system to sleep for an x number of seconds
testit <- function(x){
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}

#import shapefiles
library(sf)
library(rgdal)
shp <- readOGR(dsn=".", "latLongRadius")  #head
p_area <- data.frame(st_as_sf(shp))

#unique party names
unique_party <- as.character(unique(p_area$Party))

#create time sequence
dates <- seq.Date(from = as.Date('2020-01-10 00:00:00'), to = as.Date('2020-01-11 00:00:00'), by = 'days') 

#keywords to search in tweets
hashtags <- '#brexit + brexit'


#loop through each party name
#collect all points [Lat, Long, & radius] drawn in the area belonging to the party.
#randomly select five (5) amongst the points ('party_points')
#For each 'party_point', download n=1000 tweets that includes the 'keywords'  

#initialise (total tweets downloaded)
total_ <- 0

#tweets holder
all_Tweets <- NULL

#Given a party  
for(i in 1:length(unique_party)){ #i<-3
 
#5 random points
p_area_ <- 
    p_area %>% 
    dplyr::filter(Party==unique_party[i]) %>%
    pickRandomRows()

#loop through the selected five points   
for(j in 1:nrow(p_area_)){  #j<-1

#a point 
sub_p_area_ <- p_area_[j, ]
  
tweets_g <- searchTwitter(hashtags, n=1000, lang = "en", 
                since=as.character(dates[1]), until=as.character(dates[2]), 
                geocode=paste(sub_p_area_$long,
                              sub_p_area_$lat,
                              paste(sub_p_area_$st_lengths,"mi", sep=""), sep=",")
                              )
                              if(length(tweets_g)!=0){
                                  tweets_g <- twListToDF(tweets_g)
                                  tweets_g$longitude <- sub_p_area_$long
                                  tweets_g$latitude <- sub_p_area_$lat
                                  tweets_g <- data.frame(cbind(tweets_g, i, from=as.character(dates[1]), to=as.character(dates[2])), party_area=unique_party[i])
                                  total_ <- total_ + nrow(tweets_g)
                                  
                                  flush.console()
                                  print(paste(i, j, sep="|"))
                                  print(nrow(tweets_g))
                                  flush.console()
                                  print(total_)
      
      all_Tweets <- rbind(all_Tweets, tweets_g)
      }
}

#put system to sleep for 20 minutes after 10 calls (i.e. after data download for each party)
#this is to prevent violating '15 calls per 15minutes' API download limit.
#if(i == 2){
testit(1200)
#}

write.table(all_Tweets, 
            file=paste("C:/Users/monsu/Desktop/software paper/downloads/","download_", as.character(dates[1]), "_to_", as.character(dates[2]), "_", unique_party[i], ".csv", sep=""), 
            sep=",", row.names = F)

}







head(all_Tweets)
nrow(all_Tweets)

nc[2,]

t1 <- Sys.time()

nc[1,]$long
nc[1,]$lat
nc[1,]$radiusT


t2 <- Sys.time()
print(t2-t1)
tweets_g <- twListToDF(tweets_g)
head(tweets_g)
nrow(tweets_g)
