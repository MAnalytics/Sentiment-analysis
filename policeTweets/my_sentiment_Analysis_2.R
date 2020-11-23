#for sentiment analysis....
#has it's own cleaning function
.libPaths("C:/R/Rlib")
library(colormap)
library(fmsb)
library(tidyr)
library(radarchart)
library(grDevices)
library(htmlwidgets)
library(plotly)
library(webshot)


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
               list(name=sum)) %>%
  rename(Police.Force=Force.Name)%>%
  rename(Crime.Count=name)
  
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
library(rgdal)
library(ggplot2)
library(dplyr)
library(maptools)
library(sf)
library(reshape2)
library(scales)

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


data28 = read.table(file="./policeTweet_set_28_.csv", sep=",", head=TRUE)



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
nrow(data)
head(data)

# install.packages('rgeos', type='source')
# install.packages('rgdal', type='source')
#------------------------------------------------------
#plot of tweet volume per area
#https://medium.com/@anjesh/step-by-step-choropleth-map-in-r-a-case-of-mapping-nepal-7f62a84078d9
#https://cengel.github.io/R-spatial/mapping.html  #for color manipulation
##plot the police force area
dev.new()
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

#get names of police force with regions
Pf_names_regions <- read.table(file="Regions.csv", sep=",", head=TRUE)
Pf_names_regions_uni <- Pf_names_regions$Police.Force

#------------------------------------------------------

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

#function to remove special xters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)


#--------------------------------
for(i in 1:length(Pf_names_regions_uni)){ #i=1
#clean tweets
placeTwt <- data %>% dplyr::filter(policeForce==Pf_names_regions_uni[i]) %>%
  dplyr::select(text) %>%
  dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
  mutate(text=str_replace_all(text, "[[:punct:]]", " "))

#remove emoticons
placeTwt$text <- sapply(placeTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))

placeTwt$text <- sapply(placeTwt , fix.contractions)

#remove special characters
placeTwt$text <- sapply(placeTwt$text, removeSpecialChars)

#convert everything to lower case
placeTwt$text <- sapply(placeTwt$text, tolower)


placeTwt$text <- sapply(placeTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))


#create the data..
placeTwt_nrc <- placeTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country=Pf_names_regions_uni[i])

placeTwt_bing <- placeTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% #join with the lexicon
  mutate(country=Pf_names_regions_uni[i])

placeTwt_afinn <- placeTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("afinn")) %>% #join with the lexicon
  mutate(country=Pf_names_regions_uni[i])


#export raw data.
write.table(placeTwt, file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "cleaned_", Pf_names_regions_uni[i], "_raw", ".csv", sep=""),
           sep=",", row.names = F)



flush.console()
print(i)

write.table(placeTwt_nrc, file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "cleaned_", Pf_names_regions_uni[i], "_nrc", ".csv", sep=""),
            sep=",", row.names = F)

#write.table(placeTwt_bing, file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "cleaned_", Pf_names_regions_uni[i], "_bing", ".csv", sep=""),
#            sep=",", row.names = F)

#write.table(placeTwt_afinn, file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "cleaned_", Pf_names_regions_uni[i], "_afinn", ".csv", sep=""),
#            sep=",", row.names = F)


}



prince_bigrams <- prince_data %>%
  unnest_tokens(bigram, lyrics, token = "ngrams", n = 2)

bigrams_separated <- prince_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")






#----------------------------------------------------------------------
#Polarity chart (corrected for negations)
#----------------------------------------------------------------------

dev.new()
par(mar=rep(0.8,4))
par(mfrow=c(3,3))

#Pf_regions_uni <- unique(Pf_names_regions$Regions)

Pf_regions_uni <- c("North West", "North East","Yorkshire and the Humber",
                    "West Midlands","East Midlands","Eastern",
                    "Wales", "South West","South East")

bing_sent_Combn <- NULL


for(i in 1:length(Pf_regions_uni)){ #i=1
  
    subsetP <- Pf_names_regions %>% 
      filter(Regions == Pf_regions_uni[i])
  
    dat2 <- NULL
    raw <- NULL
    
    for(j in 1:nrow(subsetP)) {#j=1
      dat2 <- rbind(dat2, 
                    read.table(file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "cleaned_", subsetP$Police.Force[j],"_bing", ".csv", sep=""), sep=",", head=TRUE))
   
     raw <- rbind(raw, 
                    cbind(read.table(file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "cleaned_", subsetP$Police.Force[j],"_raw", ".csv", sep=""), sep=",", head=TRUE),
                    Police.Force=subsetP$Police.Force[j]))
     # raw <- raw %>%
     #   mutate(Police.Force=subsetP$Police.Force[j])
      }
      }
#}
head(raw)
summary(raw)

#get not positive of aaa
AFINN <- get_sentiments("bing")

#collate word negation 
raw_bigrams <- raw %>%
  group_by(Police.Force)%>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigrams_separated <- raw_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

not_words <- bigrams_separated %>%
  filter(word1 %in% c("not", "no", "without", "never")) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  group_by(Police.Force)%>%
  count(word2, sentiment, sort = TRUE) %>%
  ungroup() %>%
  dplyr::group_by(sentiment, Police.Force)%>%
  summarise(sum(n)) #remember, the negative will be positive and vice versa because of the negation word.

#change positive to negative and vice versa
not_words <- not_words %>%
  mutate(sentiment2=if_else(sentiment=="negative","positive1",
                 if_else(sentiment=="positive", "negative1","1")))%>%
  mutate(sentiment=if_else(sentiment2=="positive1","positive",
                 if_else(sentiment2=="negative1", "negative","1")))%>%
  dplyr::select(-c(sentiment2))

.
.
.





spike_words <- c("pandemic", "police", "policing",
                 "lockdown",
                 "corona",
                 "coronavirus",
                 "covid",
                 "covid-19",
                 "virus")

#combine all
UK_bing = data.frame(dat2[which(!dat2$word %in% spike_words),])
dat2$word
UK_bing$word

unique(UK_bing$sentiment) 
head(UK_bing)

UK_bing <-  UK_bing %>%
  count(sentiment, country) %>%
  group_by(country, sentiment) %>%
  summarise(sentiment_sum = sum(n)) %>%
  ungroup()

#add the "not" onces
not_words



.
.


bing_sent_Combn <- rbind(bing_sent_Combn, UK_bing)
#---------------------------------------------------------------------
#---------------------------------------------------------------------

UK_bing_ = UK_bing %>% 
  group_by(country) %>%
  dplyr::mutate(total=sum(sentiment_sum))%>%
  mutate(pct=round((sentiment_sum/total)*100, digits=2))

UK_bing_ = data.frame(dcast(UK_bing_, sentiment ~ country))

#write.table(UK_bing_, file="UK_bing_.csv", sep=",", row.names=F)
#write.table(UK_nrc_, file="UK_nrc_.csv", sep=",", row.names=F)

#----------------------------------------------------------------------
#Polarity chart
#---------------------------------------------------------------------
# dev.new()
# par(mar=rep(0.8,4))
# par(mfrow=c(3,3))

# Create data: note in High school for Jonathan:
UK_bing_2 = UK_bing_ %>% select(-sentiment) 
#sort as: 
max_min = rbind(rep(75, ncol(UK_bing_2)), rep(0, ncol(UK_bing_2)))
colnames(max_min) = colnames(UK_bing_2)
UK_bing_2 = rbind(max_min,UK_bing_2)
row.names(UK_bing_2)<- c("1", "2", "negative","positive")

#colors
#https://www.rapidtables.com/web/color/RGB_Color.html
# Color vector

# colors_border=c("#FF0000", rgb(0.255,0.69,0,0.9))
# colors_in=c(rgb(0.255,0,0,0.1), rgb(0.255,0.69,0,0.3))
alpha("red", 0.1)
# Set graphic colors
coul <- c("red", "forestgreen")
colors_border <- coul
#library(scales)
colors_in <- alpha(coul,0.1)
colors_in2 <- alpha(coul,0.9)


# plot with default options:
radarchart(UK_bing_2, axistype=1, seg=3,
           #custom polygon
           pcol=colors_border[1:2],
           pfcol=colors_in[1:2], plwd=2, plty=5,pch=3, 
           #custom the grid
           cglcol="grey", cglty=2, axislabcol="grey", caxislabels=seq(0,75,25), cglwd=0.1,
           #custom labels
           vlcex=1.2,
           title=Pf_regions_uni[i]
           

)
#mtext(side = 0, line = 12, at = 0, cex = 1, Pf_regions_uni[i], font = 2)
#legend(x=0.7, y=1.3, legend = rownames(UK_bing_2[-c(1,2),]), 
#       bty = "n", pch=20 , col=colors_in2[1:2], text.col = "black", cex=1.2, pt.cex=3)


}




#----------------------------------------------------------------------
#Emotion chart (corrected for negations)  ..often just look at words in isolation)
#----------------------------------------------------------------------


dev.new()
#jpeg('sentiment_rplot2.png')
par(mar=rep(0.8,4))
par(mfrow=c(3,3))

Pf_regions_uni <- unique(Pf_names_regions$Regions)

Pf_regions_uni <- c("North West", "North East","Yorkshire and the Humber",
                    "West Midlands","East Midlands","Eastern",
                    "Wales", "South West","South East")

nrc_sent_Combn <- NULL

for(i in 1:length(Pf_regions_uni)){ #i=2
  
  subsetP <- Pf_names_regions %>% 
    filter(Regions == Pf_regions_uni[i])
  
  dat2 <- NULL
  
  for(j in 1:nrow(subsetP)) {#j=1
    dat2 <- rbind(dat2, 
                  read.table(file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "cleaned_", subsetP$Police.Force[j],"_nrc", ".csv", sep=""), sep=",", head=TRUE))
    
    # dat3 <- rbind(dat3, 
    #               read.table(file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "cleaned_", subsetP$Police.Force[j],"_bing", ".csv", sep=""), sep=",", head=TRUE))
  }
  #}
  
  spike_words <- c("pandemic", "police", "policing",
                   "lockdown",
                   "corona",
                   "coronavirus",
                   "covid",
                   "covid-19",
                   "virus")
  
  #combine all
  UK_nrc = data.frame(dat2[which(!dat2$word %in% spike_words),])
  
  dat2$word
  UK_nrc$word
  
  unique(UK_nrc$sentiment) 
  head(UK_nrc)
  
  #Define some colors to use throughout
  # my_colors <- c("#E69F00", "chartreuse4", "brown", "cadetblue", "purple", "green", 
  #                "orange", "grey", "magenta", "pink", "purple", "magenta", "orange")
  
  grid.col = c(grid_,
               "positive" = "grey", 
               "negative" = "grey")
  
  UK_nrc <-  UK_nrc %>%
    filter(country != "NA" & !sentiment %in% c("positive", "negative")) %>%
    count(sentiment, country) %>%
    group_by(country, sentiment) %>%
    summarise(sentiment_sum = sum(n)) %>%
    ungroup()
  
  #---------------------------------------------------------------------
  #---------------------------------------------------------------------
  
  UK_nrc_ = UK_nrc %>% 
    group_by(country) %>%
    dplyr::mutate(total=sum(sentiment_sum))%>%
    mutate(pct=round((sentiment_sum/total)*100, digits=2))
  
  UK_nrc_ = data.frame(dcast(UK_nrc_, sentiment ~ country))
  
  #sum(UK_nrc_[,7])
  
  #keep
  if(i==1){
    nrc_sent_Combn = UK_nrc_
  }
  if(i!=1){
    nrc_sent_Combn <- cbind(nrc_sent_Combn, UK_nrc_[,2:ncol(UK_nrc_)])
  }
  #write.table(UK_bing_, file="UK_bing_.csv", sep=",", row.names=F)
  #write.table(UK_nrc_, file="UK_nrc_.csv", sep=",", row.names=F)
  
  #----------------------------------------------------------------------
  #Polarity chart
  #---------------------------------------------------------------------
  # dev.new()
  # par(mar=rep(0.8,4))
  # par(mfrow=c(3,3))
  
# Set graphic colors



UK_nrc_2 = UK_nrc_ %>% gather(Country, valname, -sentiment) %>% spread(sentiment, valname)
head(UK_nrc_2)
#sort as: 

# reference = UK_nrc_2$Country
# UK_nrc_2 <- UK_nrc_2[match(reference, UK_nrc_2$Country),]
# 
# UK_nrc_2
# #mytitle <- as.character(unlist((UK_nrc_2 %>% select(Country))))
# UK_nrc_2 = UK_nrc_2 %>% select(-Country)
# max_min = rbind(rep(24,8), rep(0, 8))
# colnames(max_min)<- c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust")
# row.names(UK_nrc_2) <- reference
# UK_nrc_2 = rbind(max_min,UK_nrc_2)


sentimentN <- colnames(UK_nrc_2)[2:length(UK_nrc_2)]
reference = UK_nrc_2$Country
UK_nrc_2 <- UK_nrc_2[match(reference, UK_nrc_2$Country),]

UK_nrc_2
#mytitle <- as.character(unlist((UK_nrc_2 %>% select(Country))))
#add_rownames(UK_nrc_2, var = UK_nrc_2$Country)

UK_nrc_2 = UK_nrc_2 %>% select(-Country)#%>%

UK_nrc_2 <- t(UK_nrc_2)

colnames(UK_nrc_2) <- reference
max_min = rbind(rep(24,length(reference)), rep(0, length(reference)))

colnames(max_min)<- reference
row.names(max_min) <- 1:2
UK_nrc_2 = data.frame(rbind(max_min,UK_nrc_2))

#row.names(UK_nrc_2) <- 1:nrow(UK_nrc_2)

library(RColorBrewer)
coul <- brewer.pal(length(sentimentN), "Set1")
colors_border <- coul
library(scales)
colors_in <- alpha(coul,0.1)
colors_in2 <- alpha(coul,0.5)
# par(mar=rep(0.3,4))
# par(mfrow=c(1,1))

# Prepare color
#colors_border= rep(adjustcolor("#00BFFF", alpha.f = 1), length(sentimentN))
#colors_in= rep(adjustcolor("#00BFFF", alpha.f = 0.2), length(sentimentN))
# plot with default options:
radarchart(UK_nrc_2, axistype=1, seg=3,
           pcol=colors_border, pfcol=colors_in, plwd=2, plty=1,
           #custom the grid
           cglcol="grey", cgltyr=1, axislabcol="grey", caxislabels=seq(0,24,8), cglwd=0.8,
           #custom labels
           vlcex=0.8,
           title=Pf_regions_uni[i])

#legend(x=0.7, y=1.3, legend = rownames(UK_nrc_2[-c(1,2),]), 
#       bty = "n", pch=20 , col=colors_in2, text.col = "black", cex=1.2, pt.cex=3)
}

#dev.copy(png,'myplot.png')




#----------------------------------------------------------------------
#Afinn chart
#----------------------------------------------------------------------

dev.new()
par(mar=rep(0.8,4))
par(mfrow=c(3,3))

#Pf_regions_uni <- unique(Pf_names_regions$Regions)

Pf_regions_uni <- c("North West", "North East","Yorkshire and the Humber",
                    "West Midlands","East Midlands","Eastern",
                    "Wales", "South West","South East")

afinn_sent_Combn <- NULL


for(i in 1:length(Pf_regions_uni)){ #i=1
  
  subsetP <- Pf_names_regions %>% 
    filter(Regions == Pf_regions_uni[i])
  
  dat2 <- NULL
  
  for(j in 1:nrow(subsetP)) {#j=1
    dat2 <- rbind(dat2, 
                  read.table(file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "cleaned_", subsetP$Police.Force[j],"_afinn", ".csv", sep=""), sep=",", head=TRUE))
  }
  #}
  
  spike_words <- c("pandemic", "police", "policing",
                   "lockdown",
                   "corona",
                   "coronavirus",
                   "covid",
                   "covid-19",
                   "virus")
  
  #combine all
  UK_afinn = data.frame(dat2[which(!dat2$word %in% spike_words),])
  dat2$word
  UK_afinn$word
  
  unique(UK_afinn$sentiment) 
  head(UK_afinn)
  
  UK_afinn <-  UK_afinn %>%
    count(sentiment, country) %>%
    group_by(country, sentiment) %>%
    summarise(sentiment_sum = sum(n)) %>%
    ungroup()
  
  afinn_sent_Combn <- rbind(afinn_sent_Combn, UK_afinn)
  #---------------------------------------------------------------------
  #---------------------------------------------------------------------
  
  UK_afinn_ = UK_afinn %>% 
    group_by(country) %>%
    dplyr::mutate(total=sum(sentiment_sum))%>%
    mutate(pct=round((sentiment_sum/total)*100, digits=2))
  
  UK_afinn_ = data.frame(dcast(UK_afinn_, sentiment ~ country))
  
  # dev.new()
  # par(mar=rep(0.8,4))
  # par(mfrow=c(3,3))
  
  # Create data: note in High school for Jonathan:
  UK_afinn_2 = UK_afinn_ %>% select(-sentiment) 
  #sort as: 
  max_min = rbind(rep(75, ncol(UK_afinn_2)), rep(0, ncol(UK_afinn_2)))
  colnames(max_min) = colnames(UK_afinn_2)
  UK_afinn_2 = rbind(max_min,UK_afinn_2)
  row.names(UK_afinn_2)<- c("1", "2", "negative","positive")
  
  #colors
  #https://www.rapidtables.com/web/color/RGB_Color.html
  # Color vector
  
  # colors_border=c("#FF0000", rgb(0.255,0.69,0,0.9))
  # colors_in=c(rgb(0.255,0,0,0.1), rgb(0.255,0.69,0,0.3))
  alpha("red", 0.1)
  # Set graphic colors
  coul <- c("red", "forestgreen")
  colors_border <- coul
  #library(scales)
  colors_in <- alpha(coul,0.1)
  colors_in2 <- alpha(coul,0.9)
  
  
  # plot with default options:
  radarchart(UK_afinn_2, axistype=1, seg=3,
             #custom polygon
             pcol=colors_border[1:2],
             pfcol=colors_in[1:2], plwd=4, plty=5,pch=3, 
             #custom the grid
             cglcol="grey", cglty=2, axislabcol="grey", caxislabels=seq(0,75,25), cglwd=0.1,
             #custom labels
             vlcex=1.2,
             title=Pf_regions_uni[i]
  )
  
  
  #mtext(side = 0, line = 12, at = 0, cex = 1, Pf_regions_uni[i], font = 2)
  #  legend(x=0.7, y=1.3, legend = rownames(UK_afinn_2[-c(1,2),]), 
  #         bty = "n", pch=20 , col=colors_in2[1:2], text.col = "black", cex=1.2, pt.cex=3)
  
  
}


#-----------------------------------------------
#Bigram for force area
#-----------------------------------------------
prince_bigrams <- prince_data %>%
  unnest_tokens(bigram, lyrics, token = "ngrams", n = 2)

bigrams_separated <- prince_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word1 %in% undesirable_words) %>%
  filter(!word2 %in% undesirable_words)

#Because there is so much repetition in music, also filter out the cases where the two words are the same
bigram_decade <- bigrams_filtered %>%
  filter(word1 != word2) %>%
  filter(decade != "NA") %>%
  unite(bigram, word1, word2, sep = " ") %>%
  inner_join(prince_data) %>%
  count(bigram, decade, sort = TRUE) %>%
  group_by(decade) %>%
  slice(seq_len(7)) %>%
  ungroup() %>%
  arrange(decade, n) %>%
  mutate(row = row_number())

#Because there is so much repetition in music, also filter out the cases where the two words are the same
bigram_decade <- bigrams_filtered %>%
  filter(word1 != word2) %>%
  filter(decade != "NA") %>%
  unite(bigram, word1, word2, sep = " ") %>%
  inner_join(prince_data) %>%
  count(bigram, decade, sort = TRUE) %>%
  group_by(decade) %>%
  slice(seq_len(7)) %>%
  ungroup() %>%
  arrange(decade, n) %>%
  mutate(row = row_number())

bigram_decade %>%
  ggplot(aes(row, n, fill = decade)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~decade, scales = "free_y") +
  xlab(NULL) + ylab(NULL) +
  scale_x_continuous(  # This handles replacement of row
    breaks = bigram_decade$row, # Notice need to reuse data frame
    labels = bigram_decade$bigram) +
  theme_lyrics() +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle("Bigrams Per Decade") +
  coord_flip()


#-----------------------------------------------
#Polar sentiment of words preceding the word "police"
#-----------------------------------------------




#-----------------------------------------------
#Police-Pandemic Bigram Network
#-----------------------------------------------







#-----------------------------------------------
#Police-Pandemic Bigram Network
#-----------------------------------------------



#creation of location-lookup table













names <- nrc_sent_Combn$sentiment
nrc_sent_Combn <- nrc_sent_Combn[,2:ncol(nrc_sent_Combn)]

nrc_sent_Combn2 <- t(nrc_sent_Combn)

Police.Force = c("Cheshire", "Cumbria", "Greater Manchester", "Lancashire", "Merseyside",
  "Cleveland", "Durham", "Northumbria",
  "Humberside","North Yorkshire", "South Yorkshire", "West Yorkshire",
  "Staffordshire", "Warwickshire", "West Mercia", "West Midlands",
  "Derbyshire", "Leicestershire","Lincolnshire","Northamptonshire","Nottinghamshire",
  "Bedfordshire", "Cambridgeshire","Essex","Hertfordshire","Norfolk","Suffolk",
  "Dyfed Powys", "Gwent", "North Wales", "South Wales",
  "Avon and Somerset", "Devon and Cornwall", "Dorset","Gloucestershire","Wiltshire",
  "Hampshire","Kent","Metropolitan Police", "Surrey","Sussex","Thames Valley")

nrc_sent_Combn2 <- data.frame(cbind(nrc_sent_Combn2, Police.Force))
row.names(nrc_sent_Combn2)<- 1:nrow(nrc_sent_Combn2)
colnames(nrc_sent_Combn2) <- c(names, "Police.Force")

lm_dat_nrc <- left_join(nrc_sent_Combn2, predict)
head(lm_dat_nrc)

model = lm(data=lm_dat_nrc, trust ~ Local.Policing + Dealing.with.the.Public + Criminal.Justice.Arrangements +
             Road.Policing+Operational.Support + Intelligence + Investigations + Public.Protection + Investigative.Support + 
             National.Policing + Support.Functions + Others)

summary(model)




#----------------------------------------------------------------
#Sentiment with Bigrams (word pairs)
#----------------------------------------------------------------

#So how do bigrams affect sentiment? This time use 
#the AFINN lexicon to perform sentiment analysis on word pairs, 
#looking at how often sentiment-associated words are preceded 
#by "not" or other negating words.


#https://www.datacamp.com/community/tutorials/sentiment-analysis-R

#How sentiment-associated words are preceded by the word "Not"






#How the word "Police" or "Policing" sentiment-associated words are preceded by the word "Police"






#Negation bigram network.



























#Explantory modellg
#---------------------------------------------------------------
#Sentiment (positive vs. negative)
#----------------------------------------------------------------

#Is the difference between police forces significant (t.test)
#https://www.kaggle.com/rtatman/tutorial-sentiment-analysis-in-r

UK_bingPN = data.frame(bing_sent_Combn)

#UK_bingPN <- UK_bing


place = unique(UK_bingPN$country)

sentimentPLACE = NULL

for(i in 1:length(place)){ #i=1

    UK_bingPN1 <- UK_bingPN %>%
      filter(country == place[i])%>%
      #count(sentiment)%>%
      spread(sentiment, sentiment_sum, fill = 0)%>% # made data wide rather than narrow
      mutate(sentiment = positive - negative) %>%
      mutate(force = place[i]) #%>%
      #mutate(category="H")
    
  sentimentPLACE <- rbind(sentimentPLACE, UK_bingPN1)  
}


sentimentPLACE <- sentimentPLACE %>%
  dplyr::select(-c(force))%>%
  dplyr::rename(Police.Force=country)


#import predictor variables.

#predict <- read.table(file="predictors.csv", sep=",", head=TRUE)
predict <- read.table(file="predictors2.csv", sep=",", head=TRUE)
#funding <- read.table(file="funding19_20.csv", sep=",", head=TRUE)
head(predict)

#join

lm_dat = left_join(sentimentPLACE, predict)
#lm_dat = left_join(lm_dat, dat_)
#lm_dat = left_join(lm_dat, funding)
colnames(lm_dat)
head(lm_dat)#
#lm_dat$sentiment

#get data
lm_dat_ = lm_dat %>%
  dplyr::select(-c(Police.Force, positive, negative))

#function to remove column with any NA value
# not_any_na <- function(x) all(!is.na(x))
# corr=data.frame(cor(lm_dat_)) #dim(corr))
# #not_na = corr %>% select_if(not_any_na)

#https://www.r-bloggers.com/2017/12/how-to-apply-linear-regression-in-r/
#Explore the response variable
#Let's check for the distribution of response variable 'medv'. 
#The following figure shows the three distributions of 'medv' original, 
#log transformation and square root transformation. 
#We can see that both 'log' and 'sqrt' does a decent job to transform 'medv' 
#distribution closer to normal. In the following model, I have selected 'log'
#transformation but it is also possible to try out 'sqrt' transformation.
##Explore the data.
ggplot(lm_dat, aes(sentiment)) + geom_density(fill="blue")
ggplot(lm_dat, aes(log(sentiment))) + geom_density(fill="blue")
ggplot(lm_dat, aes(sqrt(sentiment))) + geom_density(fill="blue")

 lm_dat_ <- lm_dat_ %>%
#   dplyr::select(-c(positive, negative, Crime.Count, Police.Force))%>%
   dplyr::filter(sentiment!=-1356)#
#   #remove outlier
#   #mutate(sentiment2 = sentiment + abs(min(sentiment)))#%>%
#   #dplyr::select(-c(sentiment))
# 
# #2865     1509
# #Model Building - Model 1
# #Now as a first step we will fit the multiple regression models. 
# #We will start by taking all input variables in the multiple regression.
# 
# ggplot(lm_dat_, aes(sentiment)) + geom_density(fill="blue")
# #ggplot(lm_dat_, aes(log(sentiment))) + geom_density(fill="blue")
# #ggplot(lm_dat_, aes(sqrt(sentiment2))) + geom_density(fill="blue")
# 
model1 = lm(sentiment~., data=lm_dat_)
summary(model1)
par(mfrow=c(2,2))
plot(model1)

lm_dat$sentiment

sqrt(sentiment)

lm_dat2 <- lm_dat %>%
  filter(sentiment!=-1356)

cor(lm_dat[,2:ncol(lm_dat)])

model = lm(data=lm_dat, sentiment ~ Local.Policing + Dealing.with.the.Public + Criminal.Justice.Arrangements +
     Road.Policing+Operational.Support + Intelligence + Investigations + Public.Protection + Investigative.Support +
     National.Policing + Support.Functions + Others ) #+ Funding)
#-----------------------------------------------------------------
summary(model)
par(mfrow=c(2,2))
plot(model)

#-----------------------------------------------------------------


#using emotional attributes
lm_dat2 = UK_nrc_2[3:nrow(UK_nrc_2), ]
lm_dat2 = data.frame(cbind(row.names(lm_dat2), lm_dat2))
row.names(lm_dat2) = 1:nrow(lm_dat2)

lm_dat2 = lm_dat2 %>%
  rename(Police.Force=row.names.lm_dat2.)

#join data
lm_dat_nrc <- left_join(lm_dat2, predict)









#plot the police force area
dev.new()
shp = readOGR(dsn=".", layer="Police_Force_Areas__December_2016__Boundaries") 
plot(shp)

shp@data









#Police and policing in the era of pandemic: A visual exploration of public sentiment using Twitter data
#Word Cloud by regions..
#word preceeding police..
#multiple linear regression..
























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


# Descriptive Statistics
# If you haven't read Part One, you may need to take a quick look at 
# a few summary graphs of the full dataset. You'll do this using 
# creative graphs from the ggplot2, circlize, and yarrr packages.
# 
# Shipshape: Word Count Per Song
# A pirate would say shipshape when everything is in good order, tidy and clean. 
# So here is an interesting view of the clean and tidy data showing the lexical diversity, 
# or, in other words, vocabulary, of the lyrics over time. A pirate plot is an advanced 
# method of plotting a continuous dependent variable, such as the word count, 
# as a function of a categorical independent variable, like decade. 
# This combines raw data points, descriptive and inferential statistics into a single effective plot. 
# Check out this great blog for more details about pirateplot() from the yarrr package.
# 
# Create the word_summary data frame that calculates the distinct word 
# count per song. The more diverse the lyrics, the larger the vocabulary. 
# Thinking about the data in this way gets you ready for word level analysis. 
# Reset the decade field to contain the value "NONE" for songs without a release 
# date and relabel those fields with cleaner labels using select().


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


my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

theme_lyrics <- function() 
{
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
}



