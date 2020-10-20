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
library(fuzzyjoin)

#read polie force location
location <- read.csv(file="PoliceForce_location.csv", sep=",", head=TRUE)
head(location)


#get names of police forces
#detecting the top highest






#-------------------------------------------------------

#new
data1 = read.table(file="./policeTweet_set_1_.csv", sep=",", head=TRUE) 
data2 = read.table(file="./policeTweet_set_2_.csv", sep=",", head=TRUE) 
data3 = read.table(file="./policeTweet_set_3_.csv", sep=",", head=TRUE) 
data4 = read.table(file="./policeTweet_set_4_.csv", sep=",", head=TRUE) 

data = rbind(data1, data2, data3, data4) #, data5, data6, data7, data8, data9, data10, 
#data11, data12, data13, data14, data15, data16, data17, data18, data19, data20,
#data21, data22, data23, data24, data25)

rm(data1, data2, data3, data4) #, data5, data6, data7, data8, data9, data10, 
#data11, data12, data13, data14, data15, data16, data17, data18, data19, data20,
#data21, data22, data23, data24, data25)
#which(duplicated(data$status_id))
#which(duplicated(data$status_id))
data$location6[1:15]
head(data)

#remove duplicates, replies, and retweets
data = data %>%
  dplyr::arrange(status_id) %>%
  dplyr::filter(!duplicated(status_id))%>%
  dplyr::filter(is.na(reply_to_status_id))%>% #removes replies
  dplyr::filter(is_retweet==FALSE)%>% #remove retweets
  dplyr::mutate(location1= gsub(",.*$", "", location)) %>% #add city name
  dplyr::select(-c(location))%>%
  dplyr::rename(location=location1)
  # mutate(location1 = str_remove(location, ", United Kingdom")) %>% #remove UK
  # mutate(location2 = str_remove(location1, ", UK")) %>% 
  # mutate(location3 = str_remove(location2, ", England")) %>% 
  # mutate(location4 = str_remove(location3, ", Wales")) %>% 
  # mutate(location5 = str_remove(location4, ", Northern Ireland")) %>% 
  # mutate(location6 = str_remove(location5, ", Scotland")) #%>% 

head(data)

data_ <- left_join(data, location, by = "location", keep=TRUE)

head(data_)
nrow(data_)


table(data_$policeForce)


write.table(data_, file="joined_Data.csv", sep=",", row.names = F)


# 
# 
# character_passages <- passages %>%
#   regex_left_join(characters, by = c(text = "character_regex"))
# 

data = data_

avonandSomersetTwt <- data %>% dplyr::filter(policeForce=="Avon and Somerset") %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " "))

essexTwt <- data %>% dplyr::filter(policeForce=="Essex") %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " "))

hampshireTwt <- data %>% dplyr::filter(policeForce=="Hampshire") %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " "))

metropolitanTwt <- data %>% dplyr::filter(policeForce=="Metropolitan") %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " ")) 

#remove emoticons
avonandSomersetTwt$text <- sapply(avonandSomersetTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
essexTwt$text <- sapply(essexTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
hampshireTwt$text <- sapply(hampshireTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
metropolitanTwt$text <- sapply(metropolitanTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))

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


avonandSomersetTwt$text <- sapply(avonandSomersetTwt , fix.contractions)
essexTwt$text <- sapply(essexTwt, fix.contractions)
hampshireTwt$text <- sapply(hampshireTwt , fix.contractions)
metropolitanTwt$text <- sapply(metropolitanTwt , fix.contractions)

# walesTwt$text <- sapply(walesTwt , fix.contractions)
# NITwt$text <- sapply(NITwt , fix.contractions)
# scotlandTwt$text <- sapply(scotlandTwt, fix.contractions)
# #head(scotlandTwt)

#function to remove special xters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters

avonandSomersetTwt$text <- sapply(avonandSomersetTwt$text, removeSpecialChars)
essexTwt$text <- sapply(essexTwt$text, removeSpecialChars)
hampshireTwt$text <- sapply(hampshireTwt$text, removeSpecialChars)
metropolitanTwt$text <- sapply(metropolitanTwt$text, removeSpecialChars)

# walesTwt$text <- sapply(walesTwt$text, removeSpecialChars)
# NITwt$text <- sapply(NITwt$text, removeSpecialChars)
# scotlandTwt$text <- sapply(scotlandTwt$text, removeSpecialChars)

# convert everything to lower case
avonandSomersetTwt$text <- sapply(avonandSomersetTwt$text, tolower)
essexTwt$text <- sapply(essexTwt$text, tolower)
hampshireTwt$text <- sapply(hampshireTwt$text, tolower)
metropolitanTwt$text <- sapply(metropolitanTwt$text, tolower)

# walesTwt$text <- sapply(walesTwt$text, tolower)
# NITwt$text <- sapply(NITwt$text, tolower)
# scotlandTwt$text <- sapply(scotlandTwt$text, tolower)
# 
# head(scotlandTwt)


head(avonandSomersetTwt)
dim(avonandSomersetTwt)
# dim(walesTwt)
# dim(NITwt)
# dim(scotlandTwt)

avonandSomersetTwt$text <- sapply(avonandSomersetTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
essexTwt$text <- sapply(essexTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
hampshireTwt$text <- sapply(hampshireTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
metropolitanTwt$text <- sapply(metropolitanTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))


# walesTwt$text <- sapply(walesTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
# NITwt$text <- sapply(NITwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
# scotlandTwt$text <- sapply(scotlandTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))






#Now go to comparison cloud
