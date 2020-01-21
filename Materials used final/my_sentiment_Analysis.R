#for sentiment analysis....
#has it's own cleaning function

library(dplyr) #Data manipulation (also included in the tidyverse package)
library(tidytext) #Text mining
library(tidyr) #Spread, separate, unite, text mining (also included in the tidyverse package)
install.packages("widyr")
###library(widyr) #Use for pairwise correlation

#Visualizations!
library(ggplot2) #Visualizations (also included in the tidyverse package)
###library(ggrepel) #`geom_label_repel`
library(gridExtra) #`grid.arrange()` for multi-graphs
library(knitr) #Create nicely formatted output tables
library(kableExtra) #Create nicely formatted output tables
library(formattable) #For the color_tile function
library(circlize) #Visualizations - chord diagram
library(memery) #Memes - images with plots
library(magick) #Memes - images with plots (image_read)
library(yarrr)  #Pirate plot
library(radarchart) #Visualizations
library(igraph) #ngram network diagrams
library(ggraph) #ngram network diagrams
library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
#library(wordcloud2) #creative visualizations
#reference: https://www.datacamp.com/community/tutorials/sentiment-analysis-R
#https://www.youtube.com/watch?v=tfuzAwXmZOk make tranparent backgroup


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
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

englandTwt$text <- sapply(englandTwt , fix.contractions)
walesTwt$text <- sapply(walesTwt , fix.contractions)
NITwt$text <- sapply(NITwt , fix.contractions)
scotlandTwt$text <- sapply(scotlandTwt, fix.contractions)
#head(scotlandTwt)

#function to remove special xters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters

englandTwt$text <- sapply(englandTwt$text, removeSpecialChars)
walesTwt$text <- sapply(walesTwt$text, removeSpecialChars)
NITwt$text <- sapply(NITwt$text, removeSpecialChars)
scotlandTwt$text <- sapply(scotlandTwt$text, removeSpecialChars)

# convert everything to lower case
englandTwt$text <- sapply(englandTwt$text, tolower)
walesTwt$text <- sapply(walesTwt$text, tolower)
NITwt$text <- sapply(NITwt$text, tolower)
scotlandTwt$text <- sapply(scotlandTwt$text, tolower)

head(scotlandTwt)
#new
# englandTwt = data.frame(text=cleanTweetsAndRemoveNAs(englandTwt))  #head(englandTwt)
# walesTwt = data.frame(text=cleanTweetsAndRemoveNAs(walesTwt$text))
# NITwt = data.frame(text=cleanTweetsAndRemoveNAs(NITwt$text))  
# scotlandTwt = data.frame(cleanTweetsAndRemoveNAs(scotlandTwt$text))  

head(englandTwt)
dim(englandTwt)

#
require(tm)
require(wordcloud)
library(formattable)
library(textdata)
library(circlize)

englandTwt$text <- sapply(englandTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
walesTwt$text <- sapply(walesTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
NITwt$text <- sapply(NITwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
scotlandTwt$text <- sapply(scotlandTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))


#create the data..
englandTwt_nrc <- englandTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="England")
head(englandTwt_nrc)

#create the data..
walesTwt_nrc <- walesTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="Wales")
head(walesTwt_nrc)

#create the data..
NITwt_nrc <- NITwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="Northern Ireland")
head(NITwt_nrc)

scotlandTwt_nrc <- scotlandTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="Scotland")
head(scotlandTwt_nrc)

#

ncol(scotlandTwt_nrc)
#combine all
UK = data.frame(rbind(englandTwt_nrc, walesTwt_nrc, NITwt_nrc, scotlandTwt_nrc))
head(UK)

unique(my_prince_nrc$sentiment) #"anticipation","fear","negative","sadness","joy",
#"positive","surprise","trust","anger","disgust

#Define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

grid.col = c("England" = my_colors[1], "Wales" = my_colors[2], "Northern Ireland" = my_colors[3], 
             "Scotland" = my_colors[4], "anger" = "grey", 
             "anticipation" = "grey", "disgust" = "grey", "fear" = "grey", 
             "joy" = "grey", "sadness" = "grey", "surprise" = "grey", "trust" = "grey")

UK_nrc <-  UK %>%
  filter(country != "NA" & !sentiment %in% c("positive", "negative")) %>%
  count(sentiment, country) %>%
  group_by(country, sentiment) %>%
  summarise(sentiment_sum = sum(n)) %>%
  ungroup()

circos.clear()
#Set the gap size
circos.par(gap.after = c(rep(5, length(unique(UK_nrc[[1]])) - 1), 15,
                         rep(5, length(unique(UK_nrc[[2]])) - 1), 15))
chordDiagram(UK_nrc, grid.col = grid.col, transparency = .2)
title("Sentiment analysis")






























head(scotlandTwt_bing)



prince_orig <- read.csv2(file="C:/Users/55131065/Documents/GitHub/Sentiment-analysis/princedata/prince_new.csv", sep=",", head=TRUE)
head(prince_orig)

prince <- prince_orig %>% 
  select(lyrics = text, song, year, album, peak, 
         us_pop = US.Pop, us_rnb = US.R.B)

#glimpse(prince[139,])
dim(prince)

#view the structure
str(prince[139, ]$lyrics, nchar.max = 300)

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
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

prince$lyrics <- sapply(prince$lyrics, fix.contractions)



prince_data <- read.csv2(file="prince_new.csv", sep=",", head=TRUE)
head(prince_data)

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

#Exploring the lexicons
#The tidytext package includes a dataset called sentiments which 
#provides several distinct lexicons. These lexicons are dictionaries 
#of words with an assigned sentiment category or value. 
#tidytext provides three general purpose lexicons:

#AFINN: assigns words with a score that runs between -5 and 5, with negative scores indicating negative sentiment and positive scores indicating positive sentiment
##Bing: assigns words into positive and negative categories
#NRC: assigns words into one or more of the following ten categories: 
#positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust

#1.create a binary (also described as polar) sentiment field for the AFINN 
#lexicon by converting the numerical score to positive or negative, 
#and add a field that holds the distinct word count for each lexicon.

#difference between lexicons
# new_sentiments <- sentiments %>% #From the tidytext package
#   dplyr::filter(lexicon != "loughran") %>% #Remove the finance lexicon
#   mutate( sentiment = ifelse(lexicon == "AFINN" & score >= 0, "positive",
#                              ifelse(lexicon == "AFINN" & score < 0,
#                                     "negative", sentiment))) %>%
#   group_by(lexicon) %>%
#   mutate(words_in_lexicon = n_distinct(word)) %>%
#   ungroup()
library(stringr)

#get the word in princes song
prince_dataTwt <- prince_data %>%
  dplyr::select(lyrics) %>%
  dplyr::mutate(lyrics = gsub("http://*|https://*|https*|\n*|*>|<*","", lyrics)) %>%
  mutate(lyrics=str_replace_all(lyrics, "[[:punct:]]", " ")) #%>%
##mutate(day=as.Date(as.character(substr(created_at,1,10)), format = "%d"))

head(prince_dataTwt)
dim(head(prince_dataTwt))

prince_dataTwt$lyrics <- sapply(prince_dataTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))

##prince_dataTwtCleaned = cleanTweetsAndRemoveNAs(prince_dataTwt)

###engTwt = paste(prince_dataTwtCleaned, collapse=" ") #w

require(tm)
require(wordcloud)
library(formattable)
library(textdata)
library(circlize)

##allTwt = removeWords(data.frame(my_word_list),stopwords("english")) #length(allTwt)


dcd <- c("1970s", "1980s", "1990s", "2000s", "2010s")

head(prince_dataTwt)

#bing prince
my_prince_bing <- prince_dataTwt %>%
  unnest_tokens(word, lyrics) %>%
  inner_join(get_sentiments("bing")) 

my_prince_bing=data.frame(cbind(my_prince_bing, dcd=sample(dcd, nrow(my_prince_bing), replace=TRUE)))

#nrc prince
my_prince_nrc <- prince_dataTwt %>%
  unnest_tokens(word, lyrics) %>%
  inner_join(get_sentiments("nrc"))

my_prince_nrc=data.frame(cbind(my_prince_nrc, dcd=sample(dcd, nrow(my_prince_nrc), replace=TRUE)))

unique(my_prince_nrc$sentiment) #"anticipation","fear","negative","sadness","joy",
#"positive","surprise","trust","anger","disgust

#Define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")

grid.col = c("1970s" = my_colors[1], "1980s" = my_colors[2], "1990s" = my_colors[3], 
             "2000s" = my_colors[4], "2010s" = my_colors[5], "anger" = "grey", 
             "anticipation" = "grey", "disgust" = "grey", "fear" = "grey", 
             "joy" = "grey", "sadness" = "grey", "surprise" = "grey", "trust" = "grey")

decade_mood_nrc <-  my_prince_nrc %>%
  filter(dcd != "NA" & !sentiment %in% c("positive", "negative")) %>%
  count(sentiment, dcd) %>%
  group_by(dcd, sentiment) %>%
  summarise(sentiment_sum = sum(n)) %>%
  ungroup()

circos.clear()
#Set the gap size
circos.par(gap.after = c(rep(5, length(unique(decade_mood_nrc[[1]])) - 1), 15,
                         rep(5, length(unique(decade_mood_nrc[[2]])) - 1), 15))
chordDiagram(decade_mood_nrc, grid.col = grid.col, transparency = .2)
title("Sentiment analysis")


#> decade_mood_nrc
# A tibble: 40 x 3
# dcd   sentiment    sentiment_sum
# <fct> <chr>                <int>
#   1 1970s anger                  729
# 2 1970s anticipation          1399
# 3 1970s disgust                520
# 4 1970s fear                   904
# 5 1970s joy                   2199
# 6 1970s sadness                915
# 7 1970s surprise               736
# 8 1970s trust                 1477
# 9 1980s anger                  720
# 10 1980s anticipation          1422
# ... with 30 more rows

#filter out 'my_word_list' not within each dictionary



# new_sentiments <- new_sentiments %>%
#   #Right join gets all words in `my_word_list` to show nulls
#   right_join(my_word_list, by = c("word" = "myword")) %>%
#   filter(word %in% my_word_list$myword)%>% #remove nas
#   filter(!is.na(sentiment))

#hh%>%group_by(sentiment) %>% summarise(n=n())




get_sentiments("nrc")



























prince$lyrics <- sapply(prince$lyrics, fix.contractions)

# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters
prince$lyrics <- sapply(prince$lyrics, removeSpecialChars)

# convert everything to lower case
prince$lyrics <- sapply(prince$lyrics, tolower)

str(prince[139, ]$lyrics, nchar.max = 300)

#--------------------------------
#create function that will remove the rest of from http.....

#create the decade column
prince <- prince %>%
  mutate(decade = 
           ifelse(prince$year %in% 1978:1979, "1970s", 
                  ifelse(prince$year %in% 1980:1989, "1980s", 
                         ifelse(prince$year %in% 1990:1999, "1990s", 
                                ifelse(prince$year %in% 2000:2009, "2000s", 
                                       ifelse(prince$year %in% 2010:2015, "2010s", 
                                              "NA"))))))

#create the chart level column
prince <- prince %>%
  mutate(chart_level = 
           ifelse(prince$peak %in% 1:10, "Top 10", 
                  ifelse(prince$peak %in% 11:100, "Top 100", "Uncharted")))

#create binary field called charted showing if a song hit the charts at all
prince <- prince %>%
  mutate(charted = 
           ifelse(prince$peak %in% 1:100, "Charted", "Uncharted"))

#save the new dataset to .csv for use in later tutorials
write.csv(prince, file = "prince_new.csv")

#read prince datasets






