#source:
#https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/
#https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/sentiment-analysis-of-twitter-data-r/  #for sentiment analysis
#get sentiment dictionary/lexicon https://www.tidytextmining.com/sentiment.html#the-sentiments-dataset 
#https://brexit.foraction.gr/
#https://towardsdatascience.com/a-guide-to-mining-and-analysing-tweets-with-r-2f56818fdd16

#https://www.r-bloggers.com/twitter-data-analysis-in-r/      #very good! equation for sentiment score.

setwd("C:/Users/monsu/Desktop/software paper/")

## load rtweet
library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
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
library(rtweet)
consumer_key <- 'rJWorDnMoARYE7OUTqaz0rOo4'
consumer_secret <- 'dUJktwlOwdbaUNB15z2Yw4HI3piOd1aTevADmathrNhBdLC3ny'
access_token <- '1108852279434715136-4IpdsZ2t69wcj0GZ3ricwL8XsU0RzT'
access_secret <- 'eX78KEUNtaU6GZ0wFay5lafhbzeZhuLzHkq71RxKnt9xj'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

token <- create_token(
  app = "UK2019",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret)

#function to pick random row in a dataframe
pickRandomRows1 = function(df, numberOfRows = 5) {
df %>% dplyr::slice(sample(1:length(df[,1]), numberOfRows, replace=FALSE))
}

#function to pick random row in a dataframe
pickRandomRows2 = function(df, numberOfRows = 2) {
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
#shp <- readOGR(dsn=".", "latLongRadius")  #head
shp <- readOGR(dsn=".", "latLongRadius")  #head
p_area <- data.frame(st_as_sf(shp))

#p_area %>% select(Party) %>% filter(Party=="Liberal Democrats")

#unique party names
unique_party <- as.character(unique(p_area$Party))

#create time sequence
dates <- seq.Date(from = as.Date('2020-01-12 00:00:00'), to = as.Date('2020-01-13 00:00:00'), by = 'days') 

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
                                  tweets_g <- data.frame(cbind(tweets_g, i, long=sub_p_area_$long, lat=sub_p_area_$lat, from=as.character(dates[1]), to=as.character(dates[2])), party_area=unique_party[i])
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

write.table(all_Tweets, 
            file=paste("C:/Users/monsu/Documents/GitHub/Sentiment-analysis/","download_", as.character(dates[1]), "_to_", as.character(dates[2]), "_", unique_party[i], ".csv", sep=""), 
            sep=",", row.names = F)

#if(i == 2){
testit(1200)
#}

}

#------------------------------------------------------
#read in a download



#53.449123, -2.3965435

write_as_csv(tweets_g, "another.csv", na="NA", fileEncoding = "UTF-8")

toDate <- format(Sys.time() - 60 * 60 * 24 * 7, "%Y%m%d%H%M")

tweets_g <- search_tweets("brexit", n=1000, type="mixed", include_rts=TRUE, 
                          token = token, lang="en",
                          fromDate="202001111420", toDate="202001141425", 
                          geocode='53.468234,-2.23912453,1mi')

tweets_g <- search_tweets("brexit", n=1000, type="mixed", include_rts=TRUE, 
                          token = token, lang="en",
                          fromDate="202001111420", toDate="202001141425", 
                          place="Leeds")

tweets_g

write_as_csv(tweets_g, "another3.csv", na="NA", fileEncoding = "UTF-8")


#point_radius:
# The point_radius: operator allows you to specify a circular geographic area and match Tweets 
#containing Tweet-specific location data that fall within that area. To use, define a central lon-lat coordinate, 
#and then set the radius (up to 25 miles). Any Tweet containing a geo Point that falls within this region will be matched. 
#Addtionally, Tweets containing Twitter Places will match where the 'geo polygon defined for the Place' falls 
#fully within the defined point-radius area. Places whose polygons fall outside the defined point-radius 
#area to any extent will not match.

