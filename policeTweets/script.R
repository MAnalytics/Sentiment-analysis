#source:
#https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/
#https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/sentiment-analysis-of-twitter-data-r/  #for sentiment analysis
#get sentiment dictionary/lexicon https://www.tidytextmining.com/sentiment.html#the-sentiments-dataset 
#https://brexit.foraction.gr/
#https://towardsdatascience.com/a-guide-to-mining-and-analysing-tweets-with-r-2f56818fdd16

#https://www.r-bloggers.com/twitter-data-analysis-in-r/      #very good! equation for sentiment score.

#setwd("C:/Users/monsu/Desktop/software paper/")
#setwd("C:/Users/monsu/Desktop/software paper/")
#setwd("C:/Users/monsu/Desktop/policeTweets/")#
setwd("C:/Users/55131065/Desktop/downloadTweets/")
#setwd("C:/R/tweets/")

getwd()

## load rtweet
library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
library(twitteR)
library(ROAuth)
library(NLP)#, lib.loc="~/R/win-library/3.3")
#library(twitteR)#, lib.loc="~/R/win-library/3.3")
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

#keywords to search in tweets
hashtags1 <- c("pandemic police",
                        "lockdown police",
                        "corona  police",
                        "coronavirus  police",
                        "covid  police",
                        "covid-19  police",
                        "covid19  police",
                        "virus  police",
                        "quarantine, police")



hashtags2 <- c("pandemic policing",
              "lockdown policing",
              "corona  policing",
              "coronavirus  policing",
              "covid  policing",
              "covid-19  policing",
              "covid19  policing",
              "virus  policing",
              "quarantine policing")


#tweets holder
all_Tweets <- NULL

#Given a party  
for(i in seq_len(length(hashtags1))){ #i<-1
  
  tweets_g1 <- NULL
  tweets_g2 <- NULL
  
    # tweets_g <- search_tweets(q=hashtags[i],  type="recent", include_rts=TRUE, 
    #                           token = token, lang="en",geocode= lookup_coords("london"),
    #                           apikey = 'AIzaSyCVb_NhAp0Ic_KEa8d7MF5RDuMobIeiuds')

    if(!i %in% c(4,5)){
    tweets_g1 <- search_tweets(q=hashtags1[i],  n=8500, type="recent", include_rts=TRUE, 
                              token = token, lang="en",geocode='53.805,-4.242,350mi')
    }
  
    if(i %in% c(4,5)){
    tweets_g1 <- search_tweets(q=hashtags1[i],  n=17500, type="recent", include_rts=TRUE, 
                               token = token, lang="en",geocode='53.805,-4.242,350mi')
    }
  
    if(nrow(tweets_g1)!=0){
      tweets_g1 <- tweets_g1 %>% dplyr::mutate(class="police")
      all_Tweets <- rbind(all_Tweets, tweets_g1)  #all_Tweets<-NULL
      #nrow(all_Tweets)
      #flush.console()
      #print(paste("hashtags1",i, sep="|"))
    }
    
    if(!i %in% c(4,5)){
    tweets_g2 <- search_tweets(q=hashtags2[i],  n=8500, type="recent", include_rts=TRUE, 
                               token = token, lang="en",geocode='53.805,-4.242,350mi')
    }
  
    if(i %in% c(4,5)){
    tweets_g2 <- search_tweets(q=hashtags2[i],  n=17500, type="recent", include_rts=TRUE, 
                               token = token, lang="en",geocode='53.805,-4.242,350mi')
    }
  
    if(nrow(tweets_g2)!=0){
      tweets_g2 <- tweets_g2 %>% dplyr::mutate(class="policing")
      all_Tweets <- rbind(all_Tweets, tweets_g2)  #all_Tweets<-NULL
      #nrow(all_Tweets)
      #flush.console()
      #print(paste("hashtags2",i, sep="|"))
    }
    
    flush.console()
    print(paste(nrow(tweets_g1), nrow(tweets_g2), sep="||"))
    print("waiting for 15.5 minutes")
    testit(960)
}


write_as_csv(all_Tweets, paste("policeTweet_set_", 61, "_.csv", sep=""), na="NA", fileEncoding = "UTF-8")









#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------



#keywords to search in tweets
hashtags1 <- c("police", "policing")


#tweets holder
all_Tweets <- NULL

#Given a party  
for(i in seq_len(length(hashtags1))){ #i<-1
  
  tweets_g1 <- NULL
  #tweets_g2 <- NULL
  
  tweets_g1 <- search_tweets(q=hashtags1[i],  n=17500, type="recent", include_rts=TRUE, 
                               token = token, lang="en",geocode='53.805,-4.242,350mi')


  if(nrow(tweets_g1)!=0){
    tweets_g1 <- tweets_g1 %>% dplyr::mutate(class=hashtags1[i])
    all_Tweets <- rbind(all_Tweets, tweets_g1)  #all_Tweets<-NULL
  }
  

  flush.console()
  print(paste(nrow(tweets_g1), nrow(tweets_g1), sep="||"))
  print("waiting for 15.5 minutes")
  testit(960)
}


write_as_csv(all_Tweets, paste("only_policeTweet_set_", 61, "_.csv", sep=""), na="NA", fileEncoding = "UTF-8")







