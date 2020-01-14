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

# #function to pick random row in a dataframe
# pickRandomRows1 = function(df, numberOfRows = 5) {
# df %>% dplyr::slice(sample(1:length(df[,1]), numberOfRows, replace=FALSE))
# }
# 
# #function to pick random row in a dataframe
# pickRandomRows2 = function(df, numberOfRows = 2) {
#   df %>% dplyr::slice(sample(1:length(df[,1]), numberOfRows, replace=FALSE))
# }


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
shp <- readOGR(dsn=".", "CirclesByVoting")  #head
p_area <- data.frame(st_as_sf(shp))

#p_area %>% select(Party) %>% filter(Party=="Liberal Democrats")

#unique party names
unique_party <- as.character(unique(p_area$Party))
unique_party <- c(unique_party[2], unique_party[1], unique_party[3], unique_party[5], unique_party[4])

#create time sequence
#dates <- seq.Date(from = as.Date('2020-01-12 00:00:00'), to = as.Date('2020-01-13 00:00:00'), by = 'days') 

#keywords to search in tweets
hashtags <- 'brexit'

#loop through each party name
#collect all points [Lat, Long, & radius] drawn in the area belonging to the party.
#randomly select five (5) amongst the points ('party_points')
#For each 'party_point', download n=1000 tweets that includes the 'keywords'  

#initialise (total tweets downloaded)
total_ <- 0

#tweets holder
all_Tweets <- NULL

#Given a party  
for(i in 1:length(unique_party)){ #i<-1
 
# if(i %in% c(1, 2, 4, 5)){
# #5 random points 
# p_area_ <- 
#     p_area %>% 
#     dplyr::filter(Party==unique_party[i]) %>%
#     pickRandomRows1()
# }
#   
# if(!i %in% c(1, 2, 4, 5)){
# #for Lib Dem
# p_area_ <- 
#   p_area %>% 
#   dplyr::filter(Party==unique_party[i]) %>%
#   pickRandomRows2()
# }

#loop through the selected five points   
for(j in 1:nrow(p_area_)){  #j<-1

#a point 
sub_p_area_ <- p_area_[j, ]

tweets_g <- search_tweets("brexit", n=5000, type="mixed", include_rts=TRUE, 
                          token = token, lang="en",
                          geocode=paste(sub_p_area_$long,
                                        sub_p_area_$lat,
                                        paste(sub_p_area_$st_lengths,"mi", sep=""), sep=",")
                                        )
      all_Tweets <- rbind(all_Tweets, tweets_g)  #all_Tweets<-NULL
      nrow(all_Tweets)
      }
#}

#put system to sleep for 20 minutes after 10 calls (i.e. after data download for each party)
#this is to prevent violating '15 calls per 15minutes' API download limit.

write_as_csv(all_Tweets, paste("C:/Users/monsu/Documents/GitHub/Sentiment-analysis/",unique_party[i],"download_0106_0107.csv", sep="_"), na="NA", fileEncoding = "UTF-8")

#if(i == 2){
testit(1200)
#}

}

getwd()
#download data by Vountry
#----------------------------------------------------
#----------------------------------------------------
#----------------------------------------------------
#----------------------------------------------------

#to download the tweet of the last 'whatever' days 
#at this first circle for the conservative
#no need to add the fromDate toDate as they seem to not doing anything
#-------------------------------------------------------------------------------
sub_p_area_ <- p_area_[j, ]

tweets_g <- search_tweets("brexit", n=5000, type="mixed", include_rts=TRUE, 
                          token = token, lang="en",
                          geocode=paste(sub_p_area_$long,
                                        sub_p_area_$lat,
                                        paste(sub_p_area_$st_lengths,"mi", sep=""), sep=",")
)
tweets_g  #all_Tweets<-NULL 121x90
min(tweets_g$created_at)  #"2020-01-06 21:53:39 UTC"
max(tweets_g$created_at)  #"2020-01-14 22:02:08 UTC"
write_as_csv(tweets_g, "download_14012020.csv", na="NA", fileEncoding = "UTF-8")

nrow(all_Tweets)
#}
#-------------------------------------------------------------------------------

write_as_csv(tweets_g, "another.csv", na="NA", fileEncoding = "UTF-8")




write.table(all_Tweets, 
            file=paste("C:/Users/monsu/Documents/GitHub/Sentiment-analysis/","download_", as.character(dates[1]), "_to_", as.character(dates[2]), "_", unique_party[i], ".csv", sep=""), 
            sep=",", row.names = F)


#53.449123, -2.3965435



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

