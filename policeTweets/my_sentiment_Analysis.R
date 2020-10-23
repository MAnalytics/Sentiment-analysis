#for sentiment analysis....
#has it's own cleaning function
.libPaths("C:/R/Rlib")
library(colormap)
library(fmsb)
library(tidyr)
library(radarchart)
library(grDevices)


#get names of police station

dat = read.table(file="C:/Users/55131065/Desktop/downloadTweets/forces2020_recordedCrimes.csv", sep=",", head=TRUE)
head(dat)
library(dplyr)
dat_ = dat %>%
  select(Force.Name, Force.outcomes.recorded.in.quarter) %>%
  filter(Force.outcomes.recorded.in.quarter!="N/a")%>%
  mutate(num=as.numeric(as.character(Force.outcomes.recorded.in.quarter))) %>%
  #filter(Force.Name == "Avon and Somerset")
  dplyr::group_by(Force.Name)%>%
  summarise_at(vars(num),
               list(name=sum))
data.frame(dat_[order(-dat_$name),])

###library(widyr) #Use for pairwise correlation

#install.packages(c("dplyr", "tidytext", "tidyr", "ggplot2", "gridExtra", "knitr", "kableExtra","formattable","circlize",
		  #"stringr", "tm","textdata"))

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

#library(wordcloud2) #creative visualizations
#reference: https://www.datacamp.com/community/tutorials/sentiment-analysis-R
#https://www.youtube.com/watch?v=tfuzAwXmZOk make tranparent backgroup
#https://jokergoo.github.io/circlize_book/book/advanced-usage-of-chorddiagram.html     #best for the chart
#https://rpubs.com/brandonkopp/creating-word-clouds-in-r

wordcloud2

#setwd("D:/IndirefTweets/scottishRef")
setwd("C:/Users/55131065/Desktop/downloadTweets/")

#read polie force location
location <- read.csv(file="PoliceForce_location.csv", sep=",", head=TRUE)
head(location)

table(location$policeForce)

#new
data1 = read.table(file="./policeTweet_set_1_.csv", sep=",", head=TRUE) 
data2 = read.table(file="./policeTweet_set_2_.csv", sep=",", head=TRUE) 
data3 = read.table(file="./policeTweet_set_3_.csv", sep=",", head=TRUE) 
data4 = read.table(file="./policeTweet_set_4_.csv", sep=",", head=TRUE) 
data5 = read.table(file="./policeTweet_set_5_.csv", sep=",", head=TRUE) 
data6 = read.table(file="./policeTweet_set_6_.csv", sep=",", head=TRUE) 
data7 = read.table(file="./policeTweet_set_7_.csv", sep=",", head=TRUE) 
data8 = read.table(file="./policeTweet_set_8_.csv", sep=",", head=TRUE)
data9 = read.table(file="./policeTweet_set_9_.csv", sep=",", head=TRUE)
data10 = read.table(file="./policeTweet_set_10_.csv", sep=",", head=TRUE)
data11 = read.table(file="./policeTweet_set_11_.csv", sep=",", head=TRUE)
data12 = read.table(file="./policeTweet_set_12_.csv", sep=",", head=TRUE)
data13 = read.table(file="./policeTweet_set_13_.csv", sep=",", head=TRUE)
data14 = read.table(file="./policeTweet_set_14_.csv", sep=",", head=TRUE)
data15 = read.table(file="./policeTweet_set_15_.csv", sep=",", head=TRUE)
data16 = read.table(file="./policeTweet_set_16_.csv", sep=",", head=TRUE)



data = rbind(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11,  
data12, data13, data14, data15, data16) #, data17, data18, data19, data20,
#data21, data22, data23, data24, data25)

rm(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, 
data11, data12, data13, data14, data15, data16) #, data17, data18, data19, data20,
#data21, data22, data23, data24, data25)
#which(duplicated(data$status_id))
#which(duplicated(data$status_id))
data$location6[1:15]
head(data)

#USE ONLY ORGANIC TWEETS >>> remove duplicate
data1 = data %>%
  dplyr::arrange(status_id) %>%
  dplyr::filter(!duplicated(status_id))%>%
  dplyr::filter(is.na(reply_to_status_id))%>% #removes replies
  dplyr::filter(is_retweet==FALSE)%>% #remove retweets
  dplyr::mutate(location1= gsub(",.*$", "", location)) %>% #add city name
  dplyr::select(-c(location))%>%
  dplyr::rename(location=location1)

#USE ONLY RETWEET 
data2 = data %>%
  dplyr::arrange(status_id) %>%
  dplyr::filter(!duplicated(status_id))%>%
  dplyr::filter(!is.na(reply_to_status_id))%>% #removes replies
  dplyr::filter(is_retweet==FALSE)%>% #remove retweets
  dplyr::mutate(location1= gsub(",.*$", "", location)) %>% #add city name
  dplyr::select(-c(location))%>%
  dplyr::rename(location=location1)

#USE BOTH ORGANIC AND REPLIES
data3 = data %>%
  dplyr::arrange(status_id) %>%
  dplyr::filter(!duplicated(status_id))%>%
  #dplyr::filter(is.na(reply_to_status_id))%>% #removes replies
  dplyr::filter(is_retweet==FALSE)%>% #remove retweets
  dplyr::mutate(location1= gsub(",.*$", "", location)) %>% #add city name
  dplyr::select(-c(location))%>%
  dplyr::rename(location=location1)


nrow(data3)

head(data)

data_1 <- left_join(data1, location, by = "location", keep=TRUE)
data_2 <- left_join(data2, location, by = "location", keep=TRUE)
data_3 <- left_join(data3, location, by = "location", keep=TRUE)


# head(data_)
# nrow(data_)
# 
# write.table(data_, file="joined_Data.csv", sep=",", row.names = F)


# character_passages <- passages %>%
#   regex_left_join(characters, by = c(text = "character_regex"))
# 

#transfer
data = data_1
data = data_2

data = data_3


# install.packages('rgeos', type='source')
# install.packages('rgdal', type='source')
#------------------------------------------------------
#plot of tweet volume per area
#https://medium.com/@anjesh/step-by-step-choropleth-map-in-r-a-case-of-mapping-nepal-7f62a84078d9
#https://cengel.github.io/R-spatial/mapping.html  #for color manipulation
##plot the police force area
#dev.new()
shp = readOGR(dsn=".", layer="Police_Force_Areas__December_2016__Boundaries",
               stringsAsFactors = FALSE)

shp@polygons[1]



#shp.sf <- st_read("Police_Force_Areas__December_2016__Boundaries.shp")

shp.sf <- st_read(system.file("shape/Police_Force_Areas__December_2016__Boundaries.shp", package="sf"))
plot(shp.sf[1])
plot(shp.sf[1][which(shp.sf$pfa16nm=="Cleveland"),], add=TRUE, color="red")
plot(shp.sf[1][which(shp.sf$pfa16nm=="Cleveland"),])


#cycle_hire_osm_projected = st_transform(shp.sf, 27700)
#plot(shp.sf)

#frequency
tab = data.frame(table(data$policeForce))
tab <- tab %>%
  dplyr::rename(Police.Force=Var1)


#read population
pop = read.table(file="population-police-force.csv", sep=",", head=TRUE)
head(pop)

pop  = left_join(pop, tab)

density = pop %>%
  mutate(density=((Freq/Mid.2010)*1000))%>%
  rename(pfa16nm = Police.Force)

#join map with density
shp.sf_density <- left_join(shp.sf, density)

library(RColorBrewer)
pal <- brewer.pal(7, "OrRd") # we select 7 colors from the palette
class(pal)

plot(shp.sf_density["density"], 
     main = "density", 
     breaks = "quantile", nbreaks = 7,
     pal = pal)

#-----------------------------------------
#plot the three.. organic, reply and total
#stack histogram





################

#plot of organic tweet
#plot of replies
#total tweets

#------------------------------------------------------

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

metropolitanTwt <- data %>% dplyr::filter(policeForce=="Metropolitan Police") %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " ")) 

southYorkshireTwt <- data %>% dplyr::filter(policeForce=="South Yorkshire") %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " ")) 

westmidlandsTwt <- data %>% dplyr::filter(policeForce=="West Midlands") %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " ")) 

westYorkshireTwt <- data %>% dplyr::filter(policeForce=="West Yorkshire") %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " ")) 

kentTwt <- data %>% dplyr::filter(policeForce=="Kent") %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " ")) 

lancashireTwt <- data %>% dplyr::filter(policeForce=="Lancashire") %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " ")) 

merseysideTwt <- data %>% dplyr::filter(policeForce=="Merseyside") %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " ")) 


cheshireTwt <- data %>% dplyr::filter(policeForce=="Cheshire") %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " ")) 

greaterManchesterTwt <- data %>% dplyr::filter(policeForce=="Greater Manchester") %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " ")) 

nottinghamshireTwt <- data %>% dplyr::filter(policeForce=="Nottinghamshire") %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " ")) 




#remove emoticons
avonandSomersetTwt$text <- sapply(avonandSomersetTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
essexTwt$text <- sapply(essexTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
hampshireTwt$text <- sapply(hampshireTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
metropolitanTwt$text <- sapply(metropolitanTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
southYorkshireTwt$text <- sapply(southYorkshireTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
westmidlandsTwt$text <- sapply(westmidlandsTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
westYorkshireTwt$text <- sapply(westYorkshireTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
kentTwt$text <- sapply(kentTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
lancashireTwt$text <- sapply(lancashireTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
merseysideTwt$text <- sapply(merseysideTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
cheshireTwt$text <- sapply(cheshireTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
greaterManchesterTwt$text <- sapply(greaterManchesterTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
nottinghamshireTwt$text <- sapply(nottinghamshireTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))


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
southYorkshireTwt$text <- sapply(southYorkshireTwt , fix.contractions)
westmidlandsTwt$text <- sapply(westmidlandsTwt , fix.contractions)
westYorkshireTwt$text <- sapply(westYorkshireTwt , fix.contractions)
kentTwt$text <- sapply(kentTwt , fix.contractions)
lancashireTwt$text <- sapply(lancashireTwt , fix.contractions)
merseysideTwt$text <- sapply(merseysideTwt , fix.contractions)
cheshireTwt$text <- sapply(cheshireTwt , fix.contractions)
greaterManchesterTwt$text <- sapply(greaterManchesterTwt , fix.contractions)
nottinghamshireTwt$text <- sapply(nottinghamshireTwt , fix.contractions)



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
southYorkshireTwt$text <- sapply(southYorkshireTwt$text, removeSpecialChars)
westmidlandsTwt$text <- sapply(westmidlandsTwt$text, removeSpecialChars)
westYorkshireTwt$text <- sapply(westYorkshireTwt$text, removeSpecialChars)
kentTwt$text <- sapply(kentTwt$text, removeSpecialChars)
lancashireTwt$text <- sapply(lancashireTwt$text, removeSpecialChars)
merseysideTwt$text <- sapply(merseysideTwt$text, removeSpecialChars)
cheshireTwt$text <- sapply(cheshireTwt$text, removeSpecialChars)
greaterManchesterTwt$text <- sapply(greaterManchesterTwt$text, removeSpecialChars)
nottinghamshireTwt$text <- sapply(nottinghamshireTwt$text, removeSpecialChars)

# walesTwt$text <- sapply(walesTwt$text, removeSpecialChars)
# NITwt$text <- sapply(NITwt$text, removeSpecialChars)
# scotlandTwt$text <- sapply(scotlandTwt$text, removeSpecialChars)

# convert everything to lower case
avonandSomersetTwt$text <- sapply(avonandSomersetTwt$text, tolower)
essexTwt$text <- sapply(essexTwt$text, tolower)
hampshireTwt$text <- sapply(hampshireTwt$text, tolower)
metropolitanTwt$text <- sapply(metropolitanTwt$text, tolower)
southYorkshireTwt$text <- sapply(southYorkshireTwt$text, tolower)
westmidlandsTwt$text <- sapply(westmidlandsTwt$text, tolower)
westYorkshireTwt$text <- sapply(westYorkshireTwt$text, tolower)
kentTwt$text <- sapply(kentTwt$text, tolower)
lancashireTwt$text <- sapply(lancashireTwt$text, tolower)
merseysideTwt$text <- sapply(merseysideTwt$text, tolower)
cheshireTwt$text <- sapply(cheshireTwt$text, tolower)
greaterManchesterTwt$text <- sapply(greaterManchesterTwt$text, tolower)
nottinghamshireTwt$text <- sapply(nottinghamshireTwt$text, tolower)

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
southYorkshireTwt$text <- sapply(southYorkshireTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
westmidlandsTwt$text <- sapply(westmidlandsTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
westYorkshireTwt$text <- sapply(westYorkshireTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
kentTwt$text <- sapply(kentTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
lancashireTwt$text <- sapply(lancashireTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
merseysideTwt$text <- sapply(merseysideTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
cheshireTwt$text <- sapply(cheshireTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
greaterManchesterTwt$text <- sapply(greaterManchesterTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
nottinghamshireTwt$text <- sapply(nottinghamshireTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))


dim(avonandSomersetTwt)
# walesTwt$text <- sapply(walesTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
# NITwt$text <- sapply(NITwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
# scotlandTwt$text <- sapply(scotlandTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))


#create the data..
avonandSomersetTwt_nrc <- avonandSomersetTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="avonandSomerset")

avonandSomersetTwt_bing <- avonandSomersetTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% #join with the lexicon
  mutate(country="avonandSomerset")


#create the data..
essexTwt_nrc <- essexTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="essex")

essexTwt_bing <- essexTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% #join with the lexicon
  mutate(country="essex")


#create the data..
hampshireTwt_nrc <- hampshireTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="hampshire")

hampshireTwt_bing <- hampshireTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% #join with the lexicon
  mutate(country="hampshire")


#create the data..
metropolitanTwt_nrc <- metropolitanTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="metropolitan")

metropolitanTwt_bing <- metropolitanTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% #join with the lexicon
  mutate(country="metropolitan")


southYorkshireTwt_nrc <- southYorkshireTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="southYorkshire")

southYorkshireTwt_bing <- southYorkshireTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% #join with the lexicon
  mutate(country="southYorkshire")

westmidlandsTwt_nrc <- westmidlandsTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="westmidlands")

westmidlandsTwt_bing <- westmidlandsTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% #join with the lexicon
  mutate(country="westmidlands")

westYorkshireTwt_nrc <- westYorkshireTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="westYorkshire")

westYorkshireTwt_bing <- westYorkshireTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% #join with the lexicon
  mutate(country="westYorkshire")

kentTwt_nrc <- kentTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="kent")

kentTwt_bing <- kentTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% #join with the lexicon
  mutate(country="kent")


lancashireTwt_nrc <- lancashireTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="lancashire")

lancashireTwt_bing <- lancashireTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% #join with the lexicon
  mutate(country="lancashire")

merseysideTwt_nrc <- merseysideTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="merseyside")

merseysideTwt_bing <- merseysideTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% #join with the lexicon
  mutate(country="merseyside")


cheshireTwt_nrc <- cheshireTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="cheshire")

cheshireTwt_bing <- cheshireTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% #join with the lexicon
  mutate(country="cheshire")

greaterManchesterTwt_nrc <- greaterManchesterTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="greaterManchester")

greaterManchesterTwt_bing <- greaterManchesterTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% #join with the lexicon
  mutate(country="greaterManchester")

nottinghamshireTwt_nrc <- nottinghamshireTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="nottinghamshire")

nottinghamshireTwt_bing <- nottinghamshireTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% #join with the lexicon
  mutate(country="nottinghamshire")




#combine all
#UK_nrc = data.frame(rbind(avonandSomersetTwt_nrc, essexTwt_nrc, NITwt_nrc, scotlandTwt_nrc))
UK_nrc = data.frame(rbind(avonandSomersetTwt_nrc, essexTwt_nrc, hampshireTwt_nrc, metropolitanTwt_nrc,
                          southYorkshireTwt_nrc, westmidlandsTwt_nrc, westYorkshireTwt_nrc, 
                          kentTwt_nrc, lancashireTwt_nrc, merseysideTwt_nrc,
                          cheshireTwt_nrc, greaterManchesterTwt_nrc, nottinghamshireTwt_nrc))

#UK_bing = data.frame(rbind(englandTwt_bing)) #, walesTwt_bing, NITwt_bing, scotlandTwt_bing))
UK_bing = data.frame(rbind(avonandSomersetTwt_bing, essexTwt_bing, hampshireTwt_bing, metropolitanTwt_bing,
                           southYorkshireTwt_bing, westmidlandsTwt_bing, westYorkshireTwt_bing, 
                           kentTwt_bing, lancashireTwt_bing, merseysideTwt_bing,
                           cheshireTwt_bing, greaterManchesterTwt_bing, nottinghamshireTwt_bing))


unique(UK_bing$sentiment) 
head(UK_nrc)
head(UK_bing)

#Define some colors to use throughout
my_colors <- c("#E69F00", "chartreuse4", "brown", "cadetblue", "purple", "green", 
               "orange", "grey", "magenta", "pink", "purple", "magenta", "orange")

grid.col = c("avonandSomerset" = my_colors[1], "essex" = my_colors[2], "hampshire" = my_colors[3], 
             "metropolitan" = my_colors[4], "southYorkshire" = my_colors[5], "westmidlands" = my_colors[6],
             "westYorkshire" = my_colors[7], "kent" = my_colors[8], "lancashire" = my_colors[9], "merseyside" = my_colors[10],
             "cheshire" = my_colors[11], "greaterManchester" = my_colors[12], "nottinghamshire" = my_colors[13],
             "positive" = "grey", 
             "negative" = "grey")

#grid.col = c("England" = my_colors[1])#, "Wales" = my_colors[2], "Northern Ireland" = my_colors[3], 
             #"Scotland" = my_colors[4], "positive" = "green", 
             #"negative" = "red")


UK_nrc <-  UK_nrc %>%
  filter(country != "NA" & !sentiment %in% c("positive", "negative")) %>%
  count(sentiment, country) %>%
  group_by(country, sentiment) %>%
  summarise(sentiment_sum = sum(n)) %>%
  ungroup()

UK_bing <-  UK_bing %>%
  count(sentiment, country) %>%
  group_by(country, sentiment) %>%
  summarise(sentiment_sum = sum(n)) %>%
  ungroup()

library(circlize)
circos.clear()

#----------------------------------nrc
#Set the gap size
circos.par(gap.after = c(rep(5, length(unique(UK_nrc[[1]])) - 1), 15,
rep(5, length(unique(UK_nrc[[2]])) - 1), 15))
chordDiagram(UK_nrc, grid.col = grid.col, transparency = .2,annotationTrackHeight = c(0.06, 0.06))
title("Sentiment analysis")


par(mfrow = c(1, 1))
circos.par(start.degree = -3)
chordDiagram(UK_bing, grid.col = grid.col, big.gap = 20,annotationTrackHeight = c(0.06, 0.06))
abline(h = 0, lty = 2, col = "#00000080")
circos.clear()

#---------------------------------bing
#circos.par(gap.after = c(rep(5, length(unique(UK_bing[[1]])) - 1), 15,
                         #rep(5, length(unique(UK_bing[[2]])) - 1), 15))
#chordDiagram(UK_bing, grid.col = grid.col, transparency = .2, annotationTrackHeight = c(0.06, 0.06))
#title("Sentiment analysis")

circos.clear()

par(mfrow = c(1, 1))
circos.par(start.degree = 0)
chordDiagram(UK_nrc, grid.col = grid.col, big.gap = 20,annotationTrackHeight = c(0.06, 0.06))
abline(h = 0, lty = 2, col = "#00000080")
circos.clear()


circos.track(track.index = 1, panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
        facing = "clockwise", niceFacing = TRUE, adj = c(0, 1))
}, bg.border = NA) 


#---------------------------------------------------------------------
#---------------------------------------------------------------------

UK_bing_ = UK_bing %>% 
		group_by(country) %>%
		dplyr::mutate(total=sum(sentiment_sum))%>%
		mutate(pct=round((sentiment_sum/total)*100, digits=2))

UK_nrc_ = UK_nrc %>% 
		group_by(country) %>%
		dplyr::mutate(total=sum(sentiment_sum))%>%
		mutate(pct=round((sentiment_sum/total)*100, digits=2))

UK_bing_ = data.frame(dcast(UK_bing_, sentiment ~ country))
UK_nrc_ = data.frame(dcast(UK_nrc_, sentiment ~ country))

write.table(UK_bing_, file="UK_bing_.csv", sep=",", row.names=F)
write.table(UK_nrc_, file="UK_nrc_.csv", sep=",", row.names=F)

#----------------------------------------------------------------------
#Polarity chart
#---------------------------------------------------------------------
par(mar=rep(0.8,4))
par(mfrow=c(1,1))

 # Create data: note in High school for Jonathan:
UK_bing_2 = UK_bing_ %>% select(-sentiment) 
#sort as: 
max_min = rbind(rep(75, 13), rep(0, 13))
colnames(max_min) = c("avonandSomerset", "essex", "hampshire", "metropolitan",
                      "southYorkshire", "westmidlands", "westYorkshire", 
                      "kent", "lancashire", "merseyside",
                      "cheshire", "greaterManchester", "nottinghamshire")
UK_bing_2 = rbind(max_min,UK_bing_2)
row.names(UK_bing_2)<- c("1", "2", "negative","positive")

#colors
#https://www.rapidtables.com/web/color/RGB_Color.html
# Color vector

colors_border=c("#FF0000", rgb(0.255,0.69,0,0.9))
colors_in=c(rgb(0.255,0,0,0.1), rgb(0.255,0.69,0,0.3))

par(mar=rep(0.8,4))
par(mfrow=c(1,1))

dev.new()
# plot with default options:
radarchart(UK_bing_2, axistype=1, seg=3,
    #custom polygon
    pcol=colors_border[1:2],
    pfcol=colors_in[1:2], plwd=4, plty=5,pch=3, 
    #custom the grid
    cglcol="grey", cglty=2, axislabcol="grey", caxislabels=seq(0,75,25), cglwd=0.1,
    #custom labels
    vlcex=1.2,
)


#----------------------------------------------------------------------
#Emotion chart
#----------------------------------------------------------------------
# Create data: note in High school for Jonathan:
UK_nrc_2 = UK_nrc_ %>% gather(Country, valname, -sentiment) %>% spread(sentiment, valname)
head(UK_nrc_2)
#sort as: 
reference = c("avonandSomerset", "essex", "hampshire", "metropolitan", 
              "southYorkshire", "westmidlands", "westYorkshire","kent", 
              "lancashire", "merseyside", "cheshire", "greaterManchester", "nottinghamshire")
UK_nrc_2 <- UK_nrc_2[match(reference, UK_nrc_2$Country),]

UK_nrc_2
mytitle <- as.character(unlist((UK_nrc_2 %>% select(Country))))
UK_nrc_2 = UK_nrc_2 %>% select(-Country)
max_min = rbind(rep(24, 8), rep(0, 8))
colnames(max_min)<- c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust")
row.names(max_min) <- 1:nrow(max_min)
UK_nrc_2 = rbind(max_min,UK_nrc_2)
row.names(UK_nrc_2) <- 1:nrow(UK_nrc_2)

# Prepare color
colors_border=c(adjustcolor("#00BFFF", alpha.f = 1), adjustcolor("#FF8C00", alpha.f = 1), 
                adjustcolor("green", alpha.f = 1), adjustcolor("#FFD700", alpha.f = 1),
                adjustcolor("violetred1", alpha.f = 1),adjustcolor("wheat4", alpha.f = 1),
                adjustcolor("slateblue", alpha.f = 1),adjustcolor("slategray4", alpha.f = 1),
                adjustcolor("slateblue", alpha.f = 1),adjustcolor("slategray4", alpha.f = 1),
                adjustcolor("slateblue", alpha.f = 1),adjustcolor("slategray4", alpha.f = 1),
                adjustcolor("slateblue", alpha.f = 1))

colors_in=c(adjustcolor("#00BFFF", alpha.f = 0.2), adjustcolor("#FF8C00", alpha.f = 0.2), 
            adjustcolor("green", alpha.f = 0.2), adjustcolor("#FFD700", alpha.f = 0.2),
            adjustcolor("violetred1", alpha.f = 0.2),adjustcolor("wheat4", alpha.f = 0.2),
            adjustcolor("slateblue", alpha.f = 0.2),adjustcolor("slategray4", alpha.f = 0.2),
            adjustcolor("slateblue", alpha.f = 0.2),adjustcolor("slategray4", alpha.f = 0.2),
            adjustcolor("slateblue", alpha.f = 0.2),adjustcolor("slategray4", alpha.f = 0.2),
            adjustcolor("slateblue", alpha.f = 0.2))


#colors_border=colormap(colormap=colormaps$viridis, nshades=4, alpha=1)
#colors_in=colormap(colormap=colormaps$viridis, nshades=4, alpha=0.3)

# Split the screen in 6 parts
par(mar=rep(0.8,4))
par(mfrow=c(4,4))

# Loop for each plot
for(i in 1:13){ #i=2
  # Custom the radarChart !
  radarchart(UK_nrc_2[c(1,2,i+2),], axistype=1, seg=3,
    #custom polygon
    pcol=colors_border[i] , pfcol=colors_in[i] , plwd=4, plty=1 , 
    #custom the grid
    cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,24,8), cglwd=0.8,
    #custom labels
    vlcex=0.8,
    #title
    title=mytitle[i]
    )
}

#----------------------------------------------------------------

#Is the difference between police forces significant (t.test)
#https://www.kaggle.com/rtatman/tutorial-sentiment-analysis-in-r

UK_bingPN = data.frame(rbind(avonandSomersetTwt_bing, essexTwt_bing, hampshireTwt_bing, metropolitanTwt_bing,
                           southYorkshireTwt_bing, westmidlandsTwt_bing, westYorkshireTwt_bing, 
                           kentTwt_bing, lancashireTwt_bing, merseysideTwt_bing,
                           cheshireTwt_bing, greaterManchesterTwt_bing, nottinghamshireTwt_bing))

place = unique(UK_bingPN$country)

sentimentPLACE = NULL

for(i in 1:length(place)){ #i=1

if(i %in% c(1:6)){
UK_bingPN1 <- UK_bingPN %>%
  filter(country == place[i])%>%
  count(sentiment)%>%
  spread(sentiment, n, fill = 0)%>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) %>%
  mutate(force = place[i]) %>%
  mutate(category="H")
}

if(i %in% c(7:13)){ 
UK_bingPN1 <- UK_bingPN %>%
  filter(country == place[i])%>%
  count(sentiment)%>%
  spread(sentiment, n, fill = 0)%>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) %>%
  mutate(force = place[i])%>%
  mutate(category="L")
}

  sentimentPLACE <- rbind(sentimentPLACE, UK_bingPN1)  
}


sentimentPLACE

#subset by region
#subset by quartile..crime volume

#is the difference between parties significant?
  # get democratic presidents & add party affiliation
  democrats <- sentimentPLACE %>%
  filter(category == c("H")) 


# get democratic presidents & party add affiliation
republicans <- sentimentPLACE %>%
  filter(category == "L")


# join both
byParty <- full_join(democrats, republicans)

# the difference between the parties is significant
t.test(democrats$sentiment, republicans$sentiment)

# plot sentiment by party
ggplot(byParty, aes(x = category, y = sentiment, color = category)) + geom_boxplot() + geom_point()




#-----------------------------------------------------------------


#plot the police force area
dev.new()
shp = readOGR(dsn=".", layer="Police_Force_Areas__December_2016__Boundaries") 
plot(shp)

shp@data

#----------------------------------------------------------

#https://cran.r-project.org/web/packages/SentimentAnalysis/vignettes/SentimentAnalysis.html
# the best
# 
# and t.test one from my laptop
# 
# 
# Word Association

#https://www.red-gate.com/simple-talk/sql/bi/text-mining-and-sentiment-analysis-with-r/

# Correlation is a statistical technique that can demonstrate whether, 
# and how strongly, pairs of variables are related. 
# This technique can be used effectively to analyze which words occur 
# most often in association with the most frequently occurring words 
# in the survey responses, which helps to see the context around these words
# 
# In your R script, add the following code and run it.

# Find associations 
findAssocs(TextDoc_dtm, terms = c("good","work","health"), corlimit = 0.25)			


#---------------------------------------------------------------------

#https://www.datacamp.com/community/tutorials/R-nlp-machine-learning

##Top Words
# In order to do a simple evaluation of the most frequently used words 
# in the full set of lyrics, you can use count() and top_n() to get 
# the n top words from your clean, filtered dataset. Then use reorder() 
# to sort words according to the count and use dplyr's mutate() verb to 
# reassign the ordered value to word. This allows ggplot() to display it nicely.
# 
prince_words_filtered %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n), fill = my_colors[4]) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Song Count") +
  ggtitle("Most Frequently Used Words in Prince Lyrics") +
  coord_flip()


##Popular Words
# So far you've just viewed the top words across all songs. 
# What happens if you break them up by chart level? 
# Are some words more prevalent in songs that reached the charts verses uncharted songs? 
# These are considered popular words by society.
# 
# Notice in the code below the use of slice(seq_len(n)) to 
# grab the first n words in each chart_level. This works differently than top_n() 
# and turns out to be a good choice when using faceting in your plot. 
# (Always remember there are different ways to do tricks like this.) 
# You can also use the row_number() function to make sure you can list the 
# words in the right order on the graph. ggplot() by default would sort words 
# alphabetically and it's good practice to do your sorting prior to graphing.
# 
popular_words <- prince_words_filtered %>% 
  group_by(chart_level) %>%
  count(word, chart_level, sort = TRUE) %>%
  slice(seq_len(8)) %>%
  ungroup() %>%
  arrange(chart_level,n) %>%
  mutate(row = row_number()) 

popular_words %>%
  ggplot(aes(row, n, fill = chart_level)) +
  geom_col(show.legend = NULL) +
  labs(x = NULL, y = "Song Count") +
  ggtitle("Popular Words by Chart Level") + 
  theme_lyrics() +  
  facet_wrap(~chart_level, scales = "free") +
  scale_x_continuous(  # This handles replacement of row 
    breaks = popular_words$row, # notice need to reuse data frame
    labels = popular_words$word) +
  coord_flip()


Descriptive Statistics
If you haven't read Part One, you may need to take a quick look at 
a few summary graphs of the full dataset. You'll do this using 
creative graphs from the ggplot2, circlize, and yarrr packages.

Shipshape: Word Count Per Song
A pirate would say shipshape when everything is in good order, tidy and clean. 
So here is an interesting view of the clean and tidy data showing the lexical diversity, 
or, in other words, vocabulary, of the lyrics over time. A pirate plot is an advanced 
method of plotting a continuous dependent variable, such as the word count, 
as a function of a categorical independent variable, like decade. 
This combines raw data points, descriptive and inferential statistics into a single effective plot. 
Check out this great blog for more details about pirateplot() from the yarrr package.

Create the word_summary data frame that calculates the distinct word 
count per song. The more diverse the lyrics, the larger the vocabulary. 
Thinking about the data in this way gets you ready for word level analysis. 
Reset the decade field to contain the value "NONE" for songs without a release 
date and relabel those fields with cleaner labels using select().


word_summary <- prince_tidy %>%
  mutate(decade = ifelse(is.na(decade),"NONE", decade)) %>%
  group_by(decade, song) %>%
  mutate(word_count = n_distinct(word)) %>%
  select(song, Released = decade, Charted = charted, word_count) %>%
  distinct() %>% #To obtain one record per song
  ungroup()

pirateplot(formula =  word_count ~ Released + Charted, #Formula
           data = word_summary, #Data frame
           xlab = NULL, ylab = "Song Distinct Word Count", #Axis labels
           main = "Lexical Diversity Per Decade", #Plot title
           pal = "google", #Color scheme
           point.o = .2, #Points
           avg.line.o = 1, #Turn on the Average/Mean line
           theme = 0, #Theme
           point.pch = 16, #Point `pch` type
           point.cex = 1.5, #Point size
           jitter.val = .1, #Turn on jitter to see the songs better
           cex.lab = .9, cex.names = .7) #Axis label size









