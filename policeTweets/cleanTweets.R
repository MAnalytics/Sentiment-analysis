.libPaths("C:/R/Rlib") #uni lap
.libPaths("C:/R") #uni deskp
##setwd("C:/Users/monsu/Desktop/downloaded tweets/scottishRef")
##setwd("C:/Users/monsu/Desktop/downloaded tweets/scottishRef")
# setwd("D:/IndirefTweets/scottishRef") #'uni lap'
# setwd("F:/IndirefTweets/scottishRef") #uni desk

#Visualizations!
library(ggplot2) #Visualizations (also included in the tidyverse package)
###library(ggrepel) #`geom_label_repel`
library(gridExtra) #`grid.arrange()` for multi-graphs
library(knitr) #Create nicely formatted output tables
library(kableExtra) #Create nicely formatted output tables
library(formattable) #For the color_tile function
library(circlize) #Visualizations - chord diagram
library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
library(stringr)
require(tm)
require(wordcloud)
library(textdata)
library(tidyr)


#new
data1 = read.table(file="./policeTweet_set_1_.csv", sep=",", head=TRUE) 
data2 = read.table(file="./policeTweet_set_2_.csv", sep=",", head=TRUE) 

data = rbind(data1, data2) #, data3, data4, data5, data6, data7, data8, data9, data10, 
#data11, data12, data13, data14, data15, data16, data17, data18, data19, data20,
#data21, data22, data23, data24, data25)

rm(data1, data2)#, data3, data4, data5, data6, data7, data8, data9, data10, 
#data11, data12, data13, data14, data15, data16, data17, data18, data19, data20,
#data21, data22, data23, data24, data25)
#which(duplicated(data$status_id))
#which(duplicated(data$status_id))

#remove duplicates, replies, and retweets
data = data %>%
  dplyr::arrange(status_id) %>%
  dplyr::filter(!duplicated(status_id))%>%
  dplyr::filter(is.na(reply_to_status_id))%>% #removes replies
  dplyr::filter(is_retweet==FALSE) #remove retweets
  
head(data)
nrow(data)


englandTwt <- data %>% #dplyr::filter(class=="England") %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " ")) #%>%
##mutate(day=as.Date(as.character(substr(created_at,1,10)), format = "%d"))

#remove emoticons
englandTwt$text <- sapply(englandTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
# walesTwt$text <- sapply(walesTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
# NITwt$text <- sapply(NITwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
# scotlandTwt$text <- sapply(scotlandTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
# #head(englandTwt)


#no need

fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  doc <- gsub("\"", " ", doc)
  doc <- gsub("\n", " ", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}


englandTwt$text <- sapply(englandTwt , fix.contractions)
# walesTwt$text <- sapply(walesTwt , fix.contractions)
# NITwt$text <- sapply(NITwt , fix.contractions)
# scotlandTwt$text <- sapply(scotlandTwt, fix.contractions)
# #head(scotlandTwt)

#function to remove special xters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters

englandTwt$text <- sapply(englandTwt$text, removeSpecialChars)
# walesTwt$text <- sapply(walesTwt$text, removeSpecialChars)
# NITwt$text <- sapply(NITwt$text, removeSpecialChars)
# scotlandTwt$text <- sapply(scotlandTwt$text, removeSpecialChars)

# convert everything to lower case
englandTwt$text <- sapply(englandTwt$text, tolower)
# walesTwt$text <- sapply(walesTwt$text, tolower)
# NITwt$text <- sapply(NITwt$text, tolower)
# scotlandTwt$text <- sapply(scotlandTwt$text, tolower)
# 
# head(scotlandTwt)


head(englandTwt)
dim(englandTwt)
# dim(walesTwt)
# dim(NITwt)
# dim(scotlandTwt)

englandTwt$text <- sapply(englandTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
# walesTwt$text <- sapply(walesTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
# NITwt$text <- sapply(NITwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
# scotlandTwt$text <- sapply(scotlandTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))






#Now go to comparison cloud
