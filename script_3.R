#source:

setwd("C:/Users/55131065/Desktop/downloadTweets/")
#setwd("C:/R/tweets/")

getwd()

## load rtweet
library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
##
library(ROAuth)
library(NLP)#, lib.loc="~/R/win-library/3.3")
#library(twitteR)#, lib.loc="~/R/win-library/3.3")
library(syuzhet)#, lib.loc="~/R/win-library/3.3")
library(tm)#, lib.loc="~/R/win-library/3.3")
library(SnowballC)#", lib.loc="~/R/win-library/3.3")
library(stringi)#", lib.loc="~/R/win-library/3.3")
library(lubridate)
library(sf)
library(rgdal)


library(twitteR) #for setting up Twitter authorization
library(rtweet) #for creating Twitter Authorization Token(S).

#setting up twitter
consumer_key <- 'myJrBpRx7c1BJIu3DotslzSIH'
consumer_secret <- 'zgIhl8CKZKhmgcYbGyWGXcF0ghYRmKi5jDnAlRcyy7daoMaBY5'
access_token <- '1108852279434715136-9pW1nBSGNkXesr86jtUqvosl0XvBWa'
access_secret <- 'AyUNX0fmtqkRygZbg9evIOvnpY13o1Pv8K78kY96CtdNU'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

Bearer_token <- 'AAAAAAAAAAAAAAAAAAAAAN2HMQEAAAAAYejjspNN8zJwkgB36EAtkxNibeg%3DMjA2z0726i8idTvwFJi3wi91Ond88K944xGVuxAV7mc6hcrbQE'
Postman_key <- 'PMAK-601c951cffc8b7004d33f279-bac60f1536703328f8236d49a8417ad0df'

token <- create_token(
  app = "OPMonitor",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret)

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


llr <- read.table(file="latlongrad.csv", sep=",", head=TRUE)
head(llr)

#keywords to search in tweets
#hashtags1 <- c("police", "policing", "law enforcement", "law enforcements")

fromDate = "201909010000"
toDate = "201909302359"
  
#tweets holder
all_Tweets <- NULL

#Given a party  
#for(i in seq_len(nrow(llr))){ #i<-1
for(i in 1:nrow(llr)){ #i<-1  
#for(i in 1:5){ #i<-1
  
  tweets_g1 <- NULL

  x=llr$lon[i]
  y=llr$lat[i]
  
  comb <- paste(paste("(police OR policing) lang:en point_radius:[",x, sep=""), paste(y, "24mi]", sep=" "), sep=" ")
  
  tweets_g1 <- search_fullarchive(q = comb,
                                  n=2000, fromDate=fromDate, toDate=toDate, 
                                  env="research", parse = TRUE,
                                  token = token)
  
  # tweets_g1 <- search_fullarchive(q = "(police OR policing) lang:en point_radius:[-5.248 50.232 24mi]",
  #                                 n=2000, fromDate=fromDate, toDate=toDate, 
  #                                 env="research", parse = TRUE,
  #                                 token = token)
  
  nrow(tweets_g1)
  # tweets_g1$text[1]
  # tweets_g2$created_at[length(tweets_g2$created_at)]
  # head(data.frame(tweets_g2))
  #tweets_g2$location
  #colnames(tweets_g1)

  if(nrow(tweets_g1)!=0){
  tweets_g1 <- tweets_g1 %>%
    dplyr::mutate(circle = i)
  all_Tweets <- rbind(all_Tweets, tweets_g1)  
  }
  
  flush.console()
  print(paste(i, nrow(tweets_g1), nrow(all_Tweets), sep="|"))
  
} 

nrow(all_Tweets)
write.table(all_Tweets, file="2019_September.csv", sep=",", row.names = F)


#}
  
  
  # flush.console()
  # print(paste(nrow(tweets_g1), nrow(tweets_g1), sep="||"))
  # print("waiting for 15.5 minutes")
  # testit(960)



write_as_csv(all_Tweets, paste("OP_policeTweet_set_", 1, "_.csv", sep=""), na="NA", fileEncoding = "UTF-8")


hw <- "Hadley Wickham"

str_sub(hw, 1, 6)






