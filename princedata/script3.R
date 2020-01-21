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



setwd("C:/Users/55131065/Documents/GitHub/Sentiment-analysis/princedata/")

library(dplyr) #data manipulation
library(ggplot2) #visualizations
library(gridExtra) #viewing multiple plots together
library(tidytext) #text mining
#library(wordcloud2) #creative visualizations

prince_orig <- read.csv2(file="prince_raw_data.csv", sep=",", head=TRUE)
summary(prince_orig)

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

prince_dataTwt$lyrics <- sapply(prince_dataTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))

##prince_dataTwtCleaned = cleanTweetsAndRemoveNAs(prince_dataTwt)

###engTwt = paste(prince_dataTwtCleaned, collapse=" ") #w

require(tm)
require(wordcloud)
library(formattable)

##allTwt = removeWords(data.frame(my_word_list),stopwords("english")) #length(allTwt)

head(allTwt)

my_word_list <- prince_dataTwt %>%
  unnest_tokens(word, lyrics) %>%
  count(word) %>%
  select(myword = word, n) %>% #Rename word
  arrange(desc(n))

#sentiment dictionary
new_sentiments <- tidytext::sentiments
head(new_sentiments)

hh <- new_sentiments %>%
  #Right join gets all words in `my_word_list` to show nulls
  right_join(my_word_list, by = c("word" = "myword")) %>%
  filter(word %in% my_word_list$myword)%>% #remove nas
  filter(!is.na(sentiment))

#hh%>%group_by(sentiment) %>% summarise(n=n())
































