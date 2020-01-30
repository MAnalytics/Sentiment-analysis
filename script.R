#source:
#https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/
#https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/sentiment-analysis-of-twitter-data-r/  #for sentiment analysis
#get sentiment dictionary/lexicon https://www.tidytextmining.com/sentiment.html#the-sentiments-dataset 
#https://brexit.foraction.gr/
#https://towardsdatascience.com/a-guide-to-mining-and-analysing-tweets-with-r-2f56818fdd16

#https://www.r-bloggers.com/twitter-data-analysis-in-r/      #very good! equation for sentiment score.

#setwd("C:/Users/monsu/Desktop/software paper/")
setwd("C:/Users/monsu/Desktop/software paper/")
setwd("C:/R/tweets/")

getwd()

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
library(sf)
library(rgdal)

#setting up twitter
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

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#scottish independence download by country


#Data download by Country
#----------------------------------------------------
#----------------------------------------------------
#----------------------------------------------------
#----------------------------------------------------
#import shapefiles
shp <- readOGR(dsn=".", "CirclesByCountry")  #head
p_area <- data.frame(st_as_sf(shp))

#unique party names
unique_country <- as.character(unique(p_area$Party))

#keywords to search in tweets
hashtags <- "#scotref OR scotref OR #indyref OR indyref OR #indyref2 OR indyref2 OR #scottishindependence OR scottishindependence"

#tweets holder
all_Tweets <- NULL

#Given a party  
for(i in 1:length(unique_country)){ #i<-1
  
  # download data from all areas of each... 
  p_area_ <- 
    p_area %>% 
    dplyr::filter(Party==unique_country[i])
  
  #----------------  
  #loop through the selected five points   
  for(j in 1:nrow(p_area_)){  #j<-1
    
    #a point 
    sub_p_area_ <- p_area_[j, ]
    
    tweets_g <- search_tweets(hashtags, n=17500, type="recent", include_rts=TRUE, 
                              token = token, lang="en",
                              geocode=paste(sub_p_area_$long,
                                            sub_p_area_$lat,
                                            paste(sub_p_area_$st_lengths,"mi", sep=""), sep=",")
    )
    if(nrow(tweets_g)!=0){
      tweets_g <- tweets_g %>% dplyr::mutate(class=unique_country[i])
      all_Tweets <- rbind(all_Tweets, tweets_g)  #all_Tweets<-NULL
      nrow(all_Tweets)
      flush.console()
      print(paste("byCountry",i, j, sep="|"))
    }
    flush.console()
    print("waiting for 15.5 minutes")
    testit(960)
  }
  
}


uniq_Dates <- unique(all_Tweets$created_at)
uniq_Dates <- uniq_Dates[order(uniq_Dates)]

write_as_csv(all_Tweets, "C:/Users/monsu/Documents/GitHub/Sentiment-analysis/scottishIndy_byCountry_TRIAL25.csv", na="NA", fileEncoding = "UTF-8")
write.table(uniq_Dates, file="C:/Users/monsu/Documents/GitHub/Sentiment-analysis/scottishIndy_byCountry_uniq_Dates_TRIAL25.csv", sep=",", row.names = F)


#write_as_csv(all_Tweets, file="C:/R/tweets/scottishIndy_byCountry_TRIAL18.csv", na="NA", fileEncoding = "UTF-8")
#write.table(uniq_Dates, file="C:/R/tweets/scottishIndy_byCountry_uniq_Dates_TRIAL18.csv", sep=",", row.names = F)

#write_as_csv(tweets_g, "try.csv", na="NA", fileEncoding = "UTF-8")
# 

head(all_Tweets)

 #testit(700)


#Data download by Voting result
#----------------------------------------------------
#----------------------------------------------------
#----------------------------------------------------
#----------------------------------------------------

#import shapefiles
shp <- readOGR(dsn=".", "CirclesByVoting")  #head
p_area <- data.frame(st_as_sf(shp))

#unique party names
unique_party <- as.character(unique(p_area$Party))
unique_party <- c(unique_party[2], unique_party[1], unique_party[3], unique_party[5], unique_party[4])

#create time sequence
#dates <- seq.Date(from = as.Date('2020-01-12 00:00:00'), to = as.Date('2020-01-13 00:00:00'), by = 'days') 

#keywords to search in tweets
hashtags <- "#brexit OR brexit"

#loop through each party name
#collect all points [Lat, Long, & radius] drawn in the area belonging to the party.
#For each 'party_point', download n=5000 tweets that includes the 'hashtags'  

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

# download data from all areas of each... 
p_area_ <- 
     p_area %>% 
     dplyr::filter(Party==unique_party[i])

#----------------  
#loop through the selected five points   
for(j in 1:nrow(p_area_)){  #j<-1
  
  #a point 
  sub_p_area_ <- p_area_[j, ]
  
  tweets_g <- search_tweets(hashtags, n=5000, type="recent", include_rts=TRUE, 
                            token = token, lang="en",
                            geocode=paste(sub_p_area_$long,
                                          sub_p_area_$lat,
                                          paste(sub_p_area_$st_lengths,"mi", sep=""), sep=",")
  )
  if(nrow(tweets_g)!=0){
    tweets_g <- tweets_g %>% dplyr::mutate(class=unique_party[i])
    all_Tweets <- rbind(all_Tweets, tweets_g)  #all_Tweets<-NULL
    nrow(all_Tweets)
    flush.console()
    print(paste("byVoting",i, j, sep="|"))
  }
  flush.console()
  print("waiting for 5 minutes")
  testit(300)
}

#put system to sleep for 20 minutes after 10 calls (i.e. after data download for each party)
#this is to prevent violating '15 calls per 15minutes' API download limit.

#put system to sleep for 10 minutes
}

uniq_Dates <- unique(all_Tweets$created_at)
uniq_Dates <- uniq_Dates[order(uniq_Dates)]

write_as_csv(all_Tweets, file="C:/Users/monsu/Documents/GitHub/Sentiment-analysis/download_byVoting_TRIAL25.csv", na="NA", fileEncoding = "UTF-8")
write.table(uniq_Dates, file="C:/Users/monsu/Documents/GitHub/Sentiment-analysis/download_byVoting_uniq_Dates_TRIAL25.csv", sep=",", row.names = F)
#w

#write_as_csv(all_Tweets, file="C:/R/tweets/download_byVoting_TRIAL15.csv", na="NA", fileEncoding = "UTF-8")
#write.table(uniq_Dates, file="C:/R/tweets/download_byVoting_uniq_Dates_TRIAL15.csv", sep=",", row.names = F)
#w




#Data download by Country
#----------------------------------------------------
#----------------------------------------------------
#----------------------------------------------------
#----------------------------------------------------
#import shapefiles
shp <- readOGR(dsn=".", "CirclesByCountry")  #head
p_area <- data.frame(st_as_sf(shp))

#unique party names
unique_country <- as.character(unique(p_area$Party))

#keywords to search in tweets
hashtags <- "#brexit OR brexit"

#loop through each party name
#collect all points [Lat, Long, & radius] drawn in the area belonging to the party.
#For each 'party_point', download n=10000 tweets that includes the 'hashtags'  

#initialise (total tweets downloaded)


#tweets holder
all_Tweets <- NULL

#Given a party  
for(i in 1:length(unique_country)){ #i<-1
  
  # download data from all areas of each... 
  p_area_ <- 
    p_area %>% 
    dplyr::filter(Party==unique_country[i])
  
  #----------------  
  #loop through the selected five points   
  for(j in 1:nrow(p_area_)){  #j<-1
    
    #a point 
    sub_p_area_ <- p_area_[j, ]
    
    tweets_g <- search_tweets(hashtags, n=17500, type="recent", include_rts=TRUE, 
                              token = token, lang="en",
                              geocode=paste(sub_p_area_$long,
                                            sub_p_area_$lat,
                                            paste(sub_p_area_$st_lengths,"mi", sep=""), sep=",")
    )
    if(nrow(tweets_g)!=0){
      tweets_g <- tweets_g %>% dplyr::mutate(class=unique_country[i])
      all_Tweets <- rbind(all_Tweets, tweets_g)  #all_Tweets<-NULL
      nrow(all_Tweets)
      flush.console()
      print(paste("byCountry",i, j, sep="|"))
    }
    flush.console()
    print("waiting for 15.5 minutes")
    testit(960)
  }
  #}
  
  #put system to sleep for 20 minutes after 10 calls (i.e. after data download for each party)
  #this is to prevent violating '15 calls per 15minutes' API download limit.
  
  #if(i == 2){
  #testit(1200)
  #}
  
}


uniq_Dates <- unique(all_Tweets$created_at)
uniq_Dates <- uniq_Dates[order(uniq_Dates)]

write_as_csv(all_Tweets, "C:/Users/monsu/Documents/GitHub/Sentiment-analysis/download_byCountry_TRIAL25.csv", na="NA", fileEncoding = "UTF-8")
write.table(uniq_Dates, file="C:/Users/monsu/Documents/GitHub/Sentiment-analysis/download_byCountry_uniq_Dates_TRIAL25.csv", sep=",", row.names = F)

# 

#write_as_csv(all_Tweets, file="C:/R/tweets/download_byCountry_TRIAL15.csv", na="NA", fileEncoding = "UTF-8")
#write.table(uniq_Dates, file="C:/R/tweets/download_byCountry_uniq_Dates_TRIAL15.csv", sep=",", row.names = F)
#wri

#put system to sleep for 10 minutes





























a=NULL
is.null(a)
is.null()

a<-c(NULL)
is.null(a)

is.null(list(1))    # FALSE (on purpose!)
is.null(integer(0))# F
is.null(logical(0))# F
as.null(list(a=1,b='c'))


a=list()
a
is.null(list())


a <- list(1:10, letters)

library(testthat)
expect_output(str(a), "List of 2")

expect_output(str(a), "int [1:9]", fixed = TRUE)

string <- "Testing is fun!"
expect_match(string, "Testing")

expect_match(string, "Testing", ignore.case = TRUE)

expect_message(), expect_warning(), expect_error()
expect_error(1 / "a", "non-numeric argument")
expect_warning(log(-1), "NaNs produced")
expect_warning(log(0))
expect_error(1 / 2) 
model <- lm(mpg ~ wt, data = mtcars)
expect_is(model, "lm")
expect_is(model, "glm")
30


base <- as.POSIXct("2009-08-03 12:01:59.23", tz = "UTC")

expect_equal(floor_date(base, "minute"), 
    as.POSIXct("2009-08-03 12:01:59", tz = "UTC"))

expect_equal(floor_date(base, "minute"), 
    as.POSIXct("2009-08-03 12:01:00", tz = "UTC"))

skip_on_cran()

expect_equal()















































london
51.507351 -0.127758
manchester 
53.480709 -2.234380

hashtags="#newwstff"

tweets_g <- search_tweets(hashtags, n=50, type="recent", include_rts=TRUE, 
                          token = token, lang="en",
                          geocode='53.480709,-2.234380,30mi')


tweets_g
write_as_csv(tweets_g, "asssssssss.csv", na="NA", fileEncoding = "UTF-8")

tweets_g$screen_name == ""

tweets_g$screen_name == ""

getwd()

#The code below is for testing 
#-------------------------------------------------------------------------------
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
getwd()
#then try tomorrow and see if the minimum will change also...like that..
#}
#-------------------------------------------------------------------------------






#to download Reka's twees

tweets_g <- search_tweets("brexit", n=10000, type="recent", include_rts=TRUE, 
                          token = token, lang="en",
                          geocode="51.52823432,-0.16123412344,100mi")

(geocode="53.45321345,-2.8651231233,100mi") #Manchester
(geocode="51.52823432,-0.16123412344,100mi") #London

tweets_g  #all_Tweets<-NULL 121x90
min(tweets_g$created_at)  #"2020-01-06 21:53:39 UTC"
max(tweets_g$created_at)  #"2020-01-14 22:02:08 UTC"
write_as_csv(tweets_g, "dsfdsdss.csv", na="NA", fileEncoding = "UTF-8")

nrow(all_Tweets)
getwd()

