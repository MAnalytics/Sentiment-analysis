
#where to buy UK basemap: https://www.canstockphoto.com/uk-countries-map-and-flags-45885601.html

#Assessing the inequality in the political sentiment - A case of #Indiref2
#Indiref2 - Examining how political sentiments vary across the United Kingdom

library(rtweet)
library(syuzhet)


#https://towardsdatascience.com/a-guide-to-mining-and-analysing-tweets-with-r-2f56818fdd16
#2. SHOW THE RATIO OF REPLIES/RETWEETS/ORGANIC TWEETS

#function for rounding off values in a dataframe
round_df <- function(x, digits) {
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

data1 = read.table(file="F:/IndirefTweets/TRIAL 2 20200116_Scot/scottishIndy_byCountry_TRIAL2.csv", sep=",", head=TRUE)
View(data1)

data2 = read.table(file="F:/IndirefTweets/TRIAL 3 20200117_Scot/scottishIndy_byCountry_TRIAL3.csv", sep=",", head=TRUE)
View(data2)

#which column in the unique field
summary(data1)
nrow(data1)
length(unique(data1$user_id))
length(unique(data1$status_id))

length(which(unique(data1$status_id) %in% unique(data2$status_id)))

length(which(unique(data1$status_id) %in% unique(data2$status_id)))

View(data1[which(data1$user_id=="x89564365"),])

write.table(g$text, file="text.txt")

View(data2[which(data2$user_id=="x89564365"),])

getwd()



#which is the unique id
#check whether we can merge both... 

# # Creating a data frame
# data <- data.frame(
#   category=c("Organic", "Retweets", "Replies"),
#   count=c(2856, 192, 120)
# )
# data

# Adding columns 
data$fraction = data$count / sum(data$count)
data$percentage = data$count / sum(data$count) * 100
data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))
# Rounding the data to two decimal points
data = round_df(data, digits = 2)
# Specify what the legend should say
Type_of_Tweet <- paste(data$category, data$percentage, "%")

#doughnut chart...
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")


#2. SHOW THE RATIO OF REPLIES/RETWEETS/ORGANIC TWEETS
#insight into participation.Conversation# engage in conversation 

# Keeping only the retweets
Gates_retweets <- Gates_tweets[Gates_tweets$is_retweet==TRUE,]
# Keeping only the replies
Gates_replies <- subset(Gates_tweets, !is.na(Gates_tweets$reply_to_status_id))

# Creating a data frame
data <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(2856, 192, 120)
)

#5. SHOW THE MOST FREQUENT WORDS FOUND IN THE TWEETS
#Data cleaning 1: remove punctuations
Gates_tweets_organic$text <-  gsub("https\\S*", "", Gates_tweets_organic$text)
Gates_tweets_organic$text <-  gsub("@\\S*", "", Gates_tweets_organic$text) 
Gates_tweets_organic$text  <-  gsub("amp", "", Gates_tweets_organic$text) 
Gates_tweets_organic$text  <-  gsub("[\r\n]", "", Gates_tweets_organic$text)
Gates_tweets_organic$text  <-  gsub("[[:punct:]]", "", Gates_tweets_organic$text)

#remove sto words.
tweets <- Gates_tweets_organic %>%
  select(text) %>%
  unnest_tokens(word, text)
tweets <- tweets %>%
  anti_join(stop_words)

#Go to script 1 for pulling all together..
tweets %>% # gives you a bar chart of the most frequent words found in the tweets
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets of Bill Gates",
       subtitle = "Stop words removed from the list")

6. SHOW THE MOST FREQUENTLY USED HASHTAGS 
Gates_tweets_organic$hashtags <- as.character(Gates_tweets_organic$hashtags)
Gates_tweets_organic$hashtags <- gsub("c\\(", "", Gates_tweets_organic$hashtags)
set.seed(1234)
wordcloud(Gates_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))#rendered as hashtags

#account where most tweet originates
set.seed(1234)
wordcloud(Gates_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

#8. PERFORM A SENTIMENT ANALYSIS OF THE TWEETS

library(syuzhet)
# Converting tweets to ASCII to trackle strange characters
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")
# removing retweets, in case needed 
tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)
# removing mentions, in case needed
tweets <-gsub("@\\w+","",tweets)
ew_sentiment<-get_nrc_sentiment((tweets))
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL
ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()
