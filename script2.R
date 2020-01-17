#Assessing the inequality in the political sentiment - A case of #Indiref2
#Indiref2 - Examining how political sentiments vary across the United Kingdom

library(rtweet)
library(syuzhet)

#https://towardsdatascience.com/a-guide-to-mining-and-analysing-tweets-with-r-2f56818fdd16
2. SHOW THE RATIO OF REPLIES/RETWEETS/ORGANIC TWEETS

#function for rounding off values in a dataframe
round_df <- function(x, digits) {
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

# Creating a data frame
data <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(2856, 192, 120)
)
data

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


2. SHOW THE RATIO OF REPLIES/RETWEETS/ORGANIC TWEETS
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

5. SHOW THE MOST FREQUENT WORDS FOUND IN THE TWEETS
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
