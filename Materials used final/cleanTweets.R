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
data9 = read.table(file="./scottishIndy_byCountry_9.csv", sep=",", head=TRUE) #29,377

data = rbind(data1, data2, data3, data4, data5, data6, data7, data8, data9)

#which(duplicated(data$status_id))
#which(duplicated(data$status_id))

#remove duplicates
data = data %>%
  dplyr::arrange(status_id) %>%
  dplyr::filter(!duplicated(status_id))
  
head(data)
nrow(data)



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
  doc <- gsub("\"", " ", doc)
  doc <- gsub("\n", " ", doc)
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


head(englandTwt)
dim(englandTwt)


englandTwt$text <- sapply(englandTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
walesTwt$text <- sapply(walesTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
NITwt$text <- sapply(NITwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
scotlandTwt$text <- sapply(scotlandTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))


#Now go to comparison cloud
