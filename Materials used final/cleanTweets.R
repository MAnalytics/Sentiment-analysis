library(stringr)
##setwd("C:/Users/monsu/Desktop/downloaded tweets/scottishRef")
##setwd("C:/Users/monsu/Desktop/downloaded tweets/scottishRef")
setwd("F:/IndirefTweets/scottishRef")


#old
load("taxiTweets.RData")
head(Meru_tweets)
#new
#get the tweets from each country
data1 = read.table(file="./scottishIndy_byCountry_1.csv", sep=",", head=TRUE) 
data2 = read.table(file="./scottishIndy_byCountry_2.csv", sep=",", head=TRUE) 
data3 = read.table(file="./scottishIndy_byCountry_3.csv", sep=",", head=TRUE) 
data4 = read.table(file="./scottishIndy_byCountry_4.csv", sep=",", head=TRUE) 
data5 = read.table(file="./scottishIndy_byCountry_5.csv", sep=",", head=TRUE) 
data6 = read.table(file="./scottishIndy_byCountry_6.csv", sep=",", head=TRUE) 
data7 = read.table(file="./scottishIndy_byCountry_7.csv", sep=",", head=TRUE) 
data8 = read.table(file="./scottishIndy_byCountry_8.csv", sep=",", head=TRUE) #29,377


data = rbind(data1, data2, data3, data4, data5, data6, data7, data8)
#which(duplicated(data$status_id))

#remove duplicates
data = data %>%
  dplyr::arrange(status_id) %>%
  dplyr::filter(!duplicated(status_id))
  
head(data)
nrow(data)

unique(data$class)

englandTwt <- data %>% dplyr::filter(class=="England") %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " ")) #%>%
  ##mutate(day=as.Date(as.character(substr(created_at,1,10)), format = "%d"))

walesTwt <- data %>% dplyr::filter(class=="Wales") %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " ")) #%>%

NITwt <- data %>% dplyr::filter(class=="Northern Ireland") %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " ")) #%>%

scotlandTwt <- data %>% dplyr::filter(class=="Scotland") %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " ")) #%>%


# # Remove retweets
# polit_tweets_organic <- polit_tweets_[polit_tweets_$isRetweet==FALSE, ]

# # Remove replies
# polit_tweets_organic <- subset(polit_tweets_organic, is.na(polit_tweets_organic$replyToSID))

#remove 'https:/....'
# englandTwt <- englandTwt %>%
#     #mutate(day=as.Date(as.character(substr(created,1,10)), format = "%d")) %>%
#     mutate(text=str_replace_all(text, "[[:punct:]]", " ")) %>%
#     mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text))

#convert to data.frame
#polit_tweets <- data.frame(polit_tweets)

#remove emoticons
englandTwt$text <- sapply(englandTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
walesTwt$text <- sapply(walesTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
NITwt$text <- sapply(NITwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
scotlandTwt$text <- sapply(scotlandTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
#head(englandTwt)

# #remove duplicate tweets..
# data("stop_words")

# get a list of words
#englandTwtCleaned <- englandTwt %>%
  #unnest_tokens(word, text) %>%
  #anti_join(stop_words) %>%
  #filter(!text %in% c("rt", "t.co"))

#no need


catch.error = function(x)
{
  # let us create a missing value for test purpose
  y = NA
  # try to catch that error (NA) we just created
  catch_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  # check result if error exists, otherwise the function works fine.
  return(y)
}

cleanTweets <- function(tweet){
  # Clean the tweet for sentiment analysis
  #  remove html links, which are not required for sentiment analysis
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  # First we will remove retweet entities from the stored tweets (text)
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  # Then remove all “#Hashtag”
  tweet = gsub("#\\w+", " ", tweet)
  # Then remove all “@people”
  tweet = gsub("@\\w+", " ", tweet)
  # Then remove all the punctuation
  tweet = gsub("[[:punct:]]", " ", tweet)
  # Then remove numbers, we need only text for analytics
  tweet = gsub("[[:digit:]]", " ", tweet)
  # finally, we remove unnecessary spaces (white spaces, tabs etc)
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  tweet = iconv(tweet, from="UTF-8", to="ASCII", sub="")
  #tweet <- iconv(s, "UTF-8", "ASCII", sub = "")
  # if anything else, you feel, should be removed, you can. For example “slang words” etc using the above function and methods.
  # Next we'll convert all the word in lowar case. This makes uniform pattern.
  tweet = catch.error(tweet)
  tweet
}

# polit_tweets_organic <- polit_tweets_organic %>% 
#     mutate(day=as.Date(as.character(substr(created,1,10)), format = "%d")) %>%
#     mutate(text=str_replace_all(text, "[[:punct:]]", " ")) %>%
#     mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text))
#   


cleanTweetsAndRemoveNAs <- function (Tweets) {
  TweetsCleaned = sapply(Tweets, cleanTweets)
  TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
  names(TweetsCleaned) = NULL
  TweetsCleaned = unique(TweetsCleaned)
  TweetsCleaned
}

#new
englandTwtCleaned = cleanTweetsAndRemoveNAs(englandTwt)  #head(englandTwtCleaned)
walesTwtCleaned = cleanTweetsAndRemoveNAs(walesTwt)  
NITwtCleaned = cleanTweetsAndRemoveNAs(NITwt)  
scotlandTwtCleaned = cleanTweetsAndRemoveNAs(scotlandTwt)  




#create the time series at school
#how can i use r r.2... 


