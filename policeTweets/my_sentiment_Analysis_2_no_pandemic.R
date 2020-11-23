#compare each month, 

#sentiment analysis reference..
#https://cran.r-project.org/web/packages/SentimentAnalysis/vignettes/SentimentAnalysis.html


rm(list=ls())

.libPaths("C:/R/Rlib")

library(colormap)
library(fmsb)
library(tidyr)
library(radarchart)
library(grDevices)
library(htmlwidgets)
library(plotly)
library(webshot)
library(rgdal)
library(sf)
library(dplyr)
library(stringr)
library(tidytext)
library(reshape2)

#to analyse tweet with about policing with no reference to pandemic.

#setwd("D:/IndirefTweets/scottishRef")
setwd("C:/Users/55131065/Desktop/downloadTweets/")

#read polie force location
location <- read.csv(file="PoliceForce_location.csv", sep=",", head=TRUE)
head(location)

table(location$policeForce)

#new (data to crop the non pandemic data from)
data28 = read.table(file="./only_policeTweet_set_28_.csv", sep=",", head=TRUE) 
data29 = read.table(file="./only_policeTweet_set_29_.csv", sep=",", head=TRUE) 
data30 = read.table(file="./only_policeTweet_set_30_.csv", sep=",", head=TRUE) 
data31 = read.table(file="./only_policeTweet_set_31_.csv", sep=",", head=TRUE) 
data32 = read.table(file="./only_policeTweet_set_32_.csv", sep=",", head=TRUE) 
data33 = read.table(file="./only_policeTweet_set_33_.csv", sep=",", head=TRUE) 
data34 = read.table(file="./only_policeTweet_set_34_.csv", sep=",", head=TRUE) 
data35 = read.table(file="./only_policeTweet_set_35_.csv", sep=",", head=TRUE) 
data36 = read.table(file="./only_policeTweet_set_36_.csv", sep=",", head=TRUE) 
data37 = read.table(file="./only_policeTweet_set_37_.csv", sep=",", head=TRUE) 
data38 = read.table(file="./only_policeTweet_set_38_.csv", sep=",", head=TRUE) 
data39 = read.table(file="./only_policeTweet_set_39_.csv", sep=",", head=TRUE) 
data40 = read.table(file="./only_policeTweet_set_40_.csv", sep=",", head=TRUE) 
data41 = read.table(file="./only_policeTweet_set_41_.csv", sep=",", head=TRUE) 
data42 = read.table(file="./only_policeTweet_set_42_.csv", sep=",", head=TRUE) 
data43 = read.table(file="./only_policeTweet_set_43_.csv", sep=",", head=TRUE) 
#data44 = read.table(file="./only_policeTweet_set_44_.csv", sep=",", head=TRUE) 
data45 = read.table(file="./only_policeTweet_set_45_.csv", sep=",", head=TRUE) 
data46 = read.table(file="./only_policeTweet_set_46_.csv", sep=",", head=TRUE) 
data47 = read.table(file="./only_policeTweet_set_47_.csv", sep=",", head=TRUE) 
data48 = read.table(file="./only_policeTweet_set_48_.csv", sep=",", head=TRUE) 
data49 = read.table(file="./only_policeTweet_set_49_.csv", sep=",", head=TRUE) 
data50 = read.table(file="./only_policeTweet_set_50_.csv", sep=",", head=TRUE) 
data51 = read.table(file="./only_policeTweet_set_51_.csv", sep=",", head=TRUE) 
data52 = read.table(file="./only_policeTweet_set_52_.csv", sep=",", head=TRUE) 
data53 = read.table(file="./only_policeTweet_set_53_.csv", sep=",", head=TRUE) 
data54 = read.table(file="./only_policeTweet_set_54_.csv", sep=",", head=TRUE) 
data55 = read.table(file="./only_policeTweet_set_55_.csv", sep=",", head=TRUE) 
data56 = read.table(file="./only_policeTweet_set_56_.csv", sep=",", head=TRUE) 
data57 = read.table(file="./only_policeTweet_set_57_.csv", sep=",", head=TRUE) 


#data to extract pandemic data from
#--------------------------------------------------------------------------------

#new (data to crop the non pandemic data from)
data28 = read.table(file="./policeTweet_set_28_.csv", sep=",", head=TRUE) 
data29 = read.table(file="./policeTweet_set_29_.csv", sep=",", head=TRUE) 
data30 = read.table(file="./policeTweet_set_30_.csv", sep=",", head=TRUE) 
data31 = read.table(file="./policeTweet_set_31_.csv", sep=",", head=TRUE) 
data32 = read.table(file="./policeTweet_set_32_.csv", sep=",", head=TRUE) 
data33 = read.table(file="./policeTweet_set_33_.csv", sep=",", head=TRUE) 
data34 = read.table(file="./policeTweet_set_34_.csv", sep=",", head=TRUE) 
data35 = read.table(file="./policeTweet_set_35_.csv", sep=",", head=TRUE) 
data36 = read.table(file="./policeTweet_set_36_.csv", sep=",", head=TRUE) 
data37 = read.table(file="./policeTweet_set_37_.csv", sep=",", head=TRUE) 
data38 = read.table(file="./policeTweet_set_38_.csv", sep=",", head=TRUE) 
data39 = read.table(file="./policeTweet_set_39_.csv", sep=",", head=TRUE) 
data40 = read.table(file="./policeTweet_set_40_.csv", sep=",", head=TRUE) 
data41 = read.table(file="./policeTweet_set_41_.csv", sep=",", head=TRUE) 
data42 = read.table(file="./policeTweet_set_42_.csv", sep=",", head=TRUE) 
data43 = read.table(file="./policeTweet_set_43_.csv", sep=",", head=TRUE) 
data44 = read.table(file="./policeTweet_set_44_.csv", sep=",", head=TRUE) 
data45 = read.table(file="./policeTweet_set_45_.csv", sep=",", head=TRUE) 
data46 = read.table(file="./policeTweet_set_46_.csv", sep=",", head=TRUE) 
data47 = read.table(file="./policeTweet_set_47_.csv", sep=",", head=TRUE) 
data48 = read.table(file="./policeTweet_set_48_.csv", sep=",", head=TRUE) 
data49 = read.table(file="./policeTweet_set_49_.csv", sep=",", head=TRUE) 
data50 = read.table(file="./policeTweet_set_50_.csv", sep=",", head=TRUE) 
data51 = read.table(file="./policeTweet_set_51_.csv", sep=",", head=TRUE) 
data52 = read.table(file="./policeTweet_set_52_.csv", sep=",", head=TRUE) 
data53 = read.table(file="./policeTweet_set_53_.csv", sep=",", head=TRUE) 
data54 = read.table(file="./policeTweet_set_54_.csv", sep=",", head=TRUE) 
data55 = read.table(file="./policeTweet_set_55_.csv", sep=",", head=TRUE) 
data56 = read.table(file="./policeTweet_set_56_.csv", sep=",", head=TRUE) 
data57 = read.table(file="./policeTweet_set_57_.csv", sep=",", head=TRUE) 

#------------------------------------------------------------------------------

data = rbind(data28, data29, data30,data31,data32,data33,data34,data35,data36,
             data37,data38,data39,data40,data41,data42,data43,data45,
             data46,data47,data48,data49,data50,data51,data52,data53,data54,
             data55,data56,data57) 

rm(data28, data29,data30,data31,data32,data33,data34,data35,data36,
   data37,data38,data39,data40,data41,data42,data43,data45,
   data46,data47,data48,data49,data50,data51,data52,data53,data54,
   data55,data56,data57) #

#write.table(data, file="data28_57.csv", sep=",", row.names=F)
#write.table(data, file="data28_57_non.csv", sep=",", row.names=F)

#saveRDS(data, file="data28_57.rds")
#saveRDS(data, file="data28_57_non.rds")
data <- readRDS(file="data28_57.rds")
data <- readRDS(file="data28_57_non.rds")

head(data)

#data58 <- = read.table(file="./only_policeTweet_set_58_.csv", sep=",", head=TRUE) 
#data <- rbind(data, data58)


#USE ONLY ORGANIC TWEETS >>> remove duplicate
# data1 = data %>%
#   dplyr::arrange(status_id) %>%
#   dplyr::filter(!duplicated(status_id))%>%
#   dplyr::filter(is.na(reply_to_status_id))%>% #removes replies
#   dplyr::filter(is_retweet==FALSE)%>% #remove retweets
#   dplyr::mutate(location1= gsub(",.*$", "", location)) %>% #add city name
#   dplyr::select(-c(location))%>%
#   dplyr::rename(location=location1)
# 
# #USE ONLY RETWEET 
# data1 = data %>%
#   dplyr::arrange(status_id) %>%
#   dplyr::filter(!duplicated(status_id))%>%
#   dplyr::filter(!is.na(reply_to_status_id))%>% #removes replies
#   dplyr::filter(is_retweet==FALSE)%>% #remove retweets
#   dplyr::mutate(location1= gsub(",.*$", "", location)) %>% #add city name
#   dplyr::select(-c(location))%>%
#   dplyr::rename(location=location1)

#USE BOTH ORGANIC AND REPLIES
data1 = data %>%
  dplyr::arrange(status_id) %>%
  dplyr::filter(!duplicated(status_id))%>%
  #dplyr::filter(is.na(reply_to_status_id))%>% #removes replies
  dplyr::filter(is_retweet==FALSE)%>% #remove retweets
  dplyr::mutate(location1= gsub(",.*$", "", location)) %>% #add city name
  dplyr::select(-c(location))%>%
  dplyr::rename(location=location1)

nrow(data)
head(data1)
nrow(data3)
data3[1:20,]


#join location to the tweets

# data_1 <- left_join(data1, location, by = "location", keep=TRUE)
# data_2 <- left_join(data2, location, by = "location", keep=TRUE)
data_1 <- left_join(data1, location, by = "location", keep=TRUE)

data_1
# head(data_)
# nrow(data_)
# 
# write.table(data_, file="joined_Data.csv", sep=",", row.names = F)


# character_passages <- passages %>%
#   regex_left_join(characters, by = c(text = "character_regex"))
# 

#transfer
# data = data_1
# data = data_2

# data = data_1
# nrow(data)

#filter data that contains any word relating to pandemic

# data <- dplyr::filter(data, !grepl(c('pandemic'),text))
# data <- dplyr::filter(data, !grepl(c('lockdown'),text))
# data <- dplyr::filter(data, !grepl(c('corona'),text))
# data <- dplyr::filter(data, !grepl(c('coronavirus'),text))
# data <- dplyr::filter(data, !grepl(c('covid'),text))
# data <- dplyr::filter(data, !grepl(c('covid-19'),text))
# data <- dplyr::filter(data, !grepl(c('virus'),text))
# data <- dplyr::filter(data, !grepl(c('quarantine'),text))

#------------------------------------------------------
#plot of tweet volume per area
#https://medium.com/@anjesh/step-by-step-choropleth-map-in-r-a-case-of-mapping-nepal-7f62a84078d9
#https://cengel.github.io/R-spatial/mapping.html  #for color manipulation
##plot the police force area
dev.new()
shp = readOGR(dsn=".", layer="Police_Force_Areas__December_2016__Boundaries",
              stringsAsFactors = FALSE)

plot(shp)
shp@polygons[1]
shp@data


#shp.sf <- st_read("Police_Force_Areas__December_2016__Boundaries.shp")

shp.sf <- st_read(system.file("shape/Police_Force_Areas__December_2016__Boundaries.shp", package="sf"))
plot(shp.sf[1])
plot(shp.sf[1][which(shp.sf$pfa16nm=="Cleveland"),], add=TRUE, color="red")
plot(shp.sf[1][which(shp.sf$pfa16nm=="Cleveland"),])


#cycle_hire_osm_projected = st_transform(shp.sf, 27700)
#plot(shp.sf)

#not very useful
#-----------------------------------------------
#frequency of what? (number of cities.)
tab = data.frame(table(data_1$policeForce))
tab <- tab %>%
  dplyr::rename(Police.Force=Var1)
tab

#read population
pop = read.table(file="population-police-force.csv", sep=",", head=TRUE)
head(pop)

pop  = left_join(pop, tab)

density = pop %>%
  mutate(density=((Freq/Mid.2010)*1000))%>%
  rename(pfa16nm = Police.Force)

#join map with density
shp.sf_density <- left_join(shp.sf, density)
#-----------------------------------------------

library(RColorBrewer)
pal <- brewer.pal(7, "OrRd") # we select 7 colors from the palette
class(pal)

# plot(shp.sf_density["density"], 
#      main = "density", 
#      breaks = "quantile", nbreaks = 7,
#      pal = pal)

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

stats <- NULL

head(data_1)
nrow(data_1)
#--------------------------------
for(i in 1:length(Pf_names_regions_uni)){ #i=1
  #clean tweets
  placeTwt <- data_1 %>% dplyr::filter(policeForce==Pf_names_regions_uni[i]) %>%
    dplyr::select(text) %>%
    dplyr::mutate(text = gsub("http://*|https://*|https*|\n*|*>|<*","", text)) %>%
    mutate(text=str_replace_all(text, "[[:punct:]]", " "))

  #more cleaning
  placeTwt <- unlist(placeTwt) %>% stringr::str_remove(pattern = "t co.*")

  #to detect the similarities
  placeTwt <- data.frame(cbind(text1=placeTwt, text2=c("NULL", placeTwt[1:length(placeTwt)-1])))
  dim(placeTwt)
  
  #similarities
  similarities <- stringsim(placeTwt$text1,placeTwt$text2,method='lcs', p=0.25)
  
  placeTwt <- placeTwt[, 1]
  dim(placeTwt)
  placeTwt <- placeTwt[which(similarities!=1)]
  
  placeTwt <- data.frame(text=placeTwt)
 
  #remove emoticons
  placeTwt$text <- sapply(placeTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
  
  placeTwt$text <- sapply(placeTwt , fix.contractions)
  
  #remove special characters
  placeTwt$text <- sapply(placeTwt$text, removeSpecialChars)
  
  #convert everything to lower case
  placeTwt$text <- sapply(placeTwt$text, tolower)
  
  
  placeTwt$text <- sapply(placeTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
  
  #keywords <- c("pandemic", "lockdown", "corona", "coronavirus","covid", "covid-19","virus","quarantine")
  
  #nrow(placeTwt)
  #here separate data into two, i.e. contain pandemic related and not pandemic
  #---------
  #tweets that contain keywords #mode(data.frame(placeTwt$text))
  placeTwt_pandemic <- data.frame(placeTwt) %>%
    dplyr::filter(stringr::str_detect(text, 'pandemic|lockdown|corona|coronavirus|covid|covid-19|virus|quarantine', negate=FALSE))
  
  placeTwt_non_p <- data.frame(placeTwt) %>%
    filter(str_detect(text, 'pandemic|lockdown|corona|coronavirus|covid|covid-19|virus|quarantine', negate=TRUE))
  
  stats <- rbind(stats, cbind(Pf_names_regions_uni[i], nrow(placeTwt_pandemic), nrow(placeTwt_non_p)))
  
  dim(placeTwt)
  #---------
  
  #create the data..
  placeTwt_pandemic_nrc <- placeTwt_pandemic %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("nrc")) %>% #join with the lexicon
    mutate(country=Pf_names_regions_uni[i])
  
  #create the data..
  placeTwt_non_p_nrc <- placeTwt_non_p %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("nrc")) %>% #join with the lexicon
    mutate(country=Pf_names_regions_uni[i])
  
  
  placeTwt_pandemic_bing <- placeTwt_pandemic %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("bing")) %>% #join with the lexicon
    mutate(country=Pf_names_regions_uni[i])
  
  placeTwt_non_p_bing <- placeTwt_non_p %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("bing")) %>% #join with the lexicon
    mutate(country=Pf_names_regions_uni[i])
  
  
  placeTwt_pandemic_afinn <- placeTwt_pandemic %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("afinn")) %>% #join with the lexicon
    mutate(country=Pf_names_regions_uni[i])
  
  placeTwt_non_p_afinn <- placeTwt_non_p %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("afinn")) %>% #join with the lexicon
    mutate(country=Pf_names_regions_uni[i])
  
  
  #export raw data.
  #write.table(placeTwt, file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "NO_PAN_cleaned_", Pf_names_regions_uni[i], "_raw", ".csv", sep=""),
              #sep=",", row.names = F)
  
  
  
  flush.console()
  print(i)
  
  #this is using the original explicit downloads..
  write.table(placeTwt_pandemic_nrc, file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "police_cleaned_", Pf_names_regions_uni[i], "_pandemic_nrc_2", ".csv", sep=""),
              sep=",", row.names = F)
  
  # write.table(placeTwt_pandemic_nrc, file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "police_cleaned_", Pf_names_regions_uni[i], "_pandemic_nrc", ".csv", sep=""),
  #             sep=",", row.names = F)
  
  # write.table(placeTwt_non_p_nrc, file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "police_cleaned_", Pf_names_regions_uni[i], "_non_p_nrc", ".csv", sep=""),
  #             sep=",", row.names = F)
  # 
  

  write.table(placeTwt_pandemic_bing, file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "police_cleaned_", Pf_names_regions_uni[i], "_pandemic_bing_2", ".csv", sep=""),
              sep=",", row.names = F)
  
  # write.table(placeTwt_pandemic_bing, file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "police_cleaned_", Pf_names_regions_uni[i], "_pandemic_bing", ".csv", sep=""),
  #             sep=",", row.names = F)
  # write.table(placeTwt_non_p_bing, file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "police_cleaned_", Pf_names_regions_uni[i], "_non_p_bing", ".csv", sep=""),
  #             sep=",", row.names = F)
  # 
  

  write.table(placeTwt_pandemic_afinn, file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "police_cleaned_", Pf_names_regions_uni[i], "_pandemic_afinn_2", ".csv", sep=""),
              sep=",", row.names = F)
  
  # write.table(placeTwt_pandemic_afinn, file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "police_cleaned_", Pf_names_regions_uni[i], "_pandemic_afinn", ".csv", sep=""),
  #             sep=",", row.names = F)
  
  # write.table(placeTwt_non_p_afinn, file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "police_cleaned_", Pf_names_regions_uni[i], "_non_p_afinn", ".csv", sep=""),
  #             sep=",", row.names = F)
  # 
  
}

stats_ <- data.frame(stats)
colnames(stats_) <- c("PoliceForce","pandemic_tw","non_pandemic_tw")

stats_$pandemic_tw <- as.numeric(stats_$pandemic_tw)
stats_$non_pandemic_tw <- as.numeric(stats_$non_pandemic_tw)

stats_ %>% 
  dplyr::mutate(perc_pan = round((pandemic_tw/(pandemic_tw + non_pandemic_tw))*100, digits=1)) %>%
  dplyr::mutate(perc_non_pan = round((non_pandemic_tw/(pandemic_tw + non_pandemic_tw))*100, digits=1)) 

 

mode(stats$pandemic_tw)

# prince_bigrams <- prince_data %>%
#   unnest_tokens(bigram, lyrics, token = "ngrams", n = 2)
# 
# bigrams_separated <- prince_bigrams %>%
#   separate(bigram, c("word1", "word2"), sep = " ")
# 



#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#Polarity chart (to be corrected for negations)  ..often just look at words in isolation)
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------



# dev.new()
# #jpeg('sentiment_rplot2.png')
# par(mar=rep(0.8,4))
# par(mfrow=c(9,2))

dev.new()
#jpeg('sentiment_rplot2.png')
par(mar=rep(0.2,4))
par(mfrow=c(9,2))


Pf_regions_uni <- unique(Pf_names_regions$Regions)

Pf_regions_uni <- c("North West", "North East","Yorkshire and the Humber",
                    "West Midlands","East Midlands","Eastern",
                    "Wales", "South West","South East")

nrc_sent_Combn <- NULL
nrc_sent_Combn_2 <- NULL

for(i in 1:length(Pf_regions_uni)){ #i=1
  
  subsetP <- Pf_names_regions %>% 
    filter(Regions == Pf_regions_uni[i])
  
  Region <- Pf_regions_uni[i]
  
  UK_bing_combined <- NULL
  
  dat2 <- NULL
  dat2_2 <- NULL
  
  for(j in 1:nrow(subsetP)) {#j=1
    
    #this use pandemic reference data with download set 1
    dat2 <- read.table(file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "police_cleaned_", subsetP$Police.Force[j],"_pandemic_bing_2", ".csv", sep=""), sep=",", head=TRUE)
    
    #this use the non pandemic reference data of download set 2
    dat2_2 <- read.table(file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "police_cleaned_", subsetP$Police.Force[j],"_non_p_bing", ".csv", sep=""), sep=",", head=TRUE)
    
    # dat2 <- read.table(file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "police_cleaned_", subsetP$Police.Force[j],"_pandemic_bing", ".csv", sep=""), sep=",", head=TRUE)
    # 
    # dat2_2 <- read.table(file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "police_cleaned_", subsetP$Police.Force[j],"_non_p_bing", ".csv", sep=""), sep=",", head=TRUE)
    # 
    spike_words <- c("pandemic", "police", "policing",
                     "lockdown",
                     "corona",
                     "coronavirus",
                     "covid",
                     "covid-19",
                     "covid19",
                     "virus",
                     "quarantine")
    
    #combine all
    UK_bing = data.frame(dat2[which(!dat2$word %in% spike_words),])
    #combine all
    UK_bing_2 = data.frame(dat2_2[which(!dat2_2$word %in% spike_words),])
    
    
    UK_bing <-  UK_bing %>%
      #filter(country != "NA" & !sentiment %in% c("positive", "negative")) %>%
      count(sentiment, country) %>%
      group_by(country, sentiment) %>%
      summarise(sentiment_sum = sum(n)) %>%
      ungroup()
    
    UK_bing_2 <-  UK_bing_2 %>%
      #filter(country != "NA" & !sentiment %in% c("positive", "negative")) %>%
      count(sentiment, country) %>%
      group_by(country, sentiment) %>%
      summarise(sentiment_sum = sum(n)) %>%
      ungroup()
    #---------------------------------------------------------------------
    #---------------------------------------------------------------------
    
    UK_bing_ = UK_bing %>% 
      group_by(country) %>%
      dplyr::mutate(total=sum(sentiment_sum))%>%
      mutate(pct=round((sentiment_sum/total)*100, digits=2))
    
    UK_bing_ = data.frame(dcast(UK_bing_, sentiment ~ country))
    
    UK_bing__2 = UK_bing_2 %>% 
      group_by(country) %>%
      dplyr::mutate(total=sum(sentiment_sum))%>%
      mutate(pct=round((sentiment_sum/total)*100, digits=2))
    
    UK_bing__2 = data.frame(dcast(UK_bing__2, sentiment ~ country))
  
    # Set graphic colors
    
    UK_bing_2 = UK_bing_ %>% gather(Country, valname, -sentiment) %>% 
      spread(sentiment, valname)%>%
      dplyr::rename(negative_P_ref = negative)%>%
      dplyr::rename(positive_P_ref=positive)
    
    UK_bing_2_2 = UK_bing__2 %>% gather(Country, valname, -sentiment) %>% 
      spread(sentiment, valname)%>%
      dplyr::rename(negative_noP_ref = negative)%>%
      dplyr::rename(positive_noP_ref=positive)
    
    #join
    UK_bing_combined <- rbind(UK_bing_combined,
                              cbind(UK_bing_2, UK_bing_2_2[,2:ncol(UK_bing_2_2)]))
    
  }
    
      sentimentN <- colnames(UK_bing_combined)[2:length(UK_bing_combined)]
      reference = UK_bing_combined$Country
      UK_bing_combined <- UK_bing_combined[match(reference, UK_bing_combined$Country),]
      
      UK_bing_combined = UK_bing_combined %>% select(-Country)#%>%
      
      UK_bing_combined <- t(UK_bing_combined)
      
      
      colnames(UK_bing_combined) <- reference
      max_min = rbind(rep(24,length(colnames(UK_bing_combined))), 
                      rep(0, length(colnames(UK_bing_combined))))
      rownames(max_min)<- 1:2
      colnames(max_min) <- colnames(UK_bing_combined)
      UK_bing_combined = data.frame(rbind(max_min,UK_bing_combined))
      
      
      UK_bing_combined_1 <- UK_bing_combined %>% 
        dplyr::filter(row.names(UK_bing_combined) %in% c("1","2","positive_P_ref", "positive_noP_ref"))
      UK_bing_combined_1[1,] <- 40
      
      UK_bing_combined_2 <- UK_bing_combined %>% 
        dplyr::filter(row.names(UK_bing_combined) %in% c("1","2","negative_P_ref", "negative_noP_ref")) 
      UK_bing_combined_2[1,] <- 60
      
      library(RColorBrewer)
      #coul <- brewer.pal(5, "Set1")
      coul1 <- c("#377EB8", "#377EB8")
      coul2 <- c("#E41A1C", "#E41A1C")
      colors_border1 <- coul1
      colors_border2 <- coul2
      library(scales)
      
      colors_in1 <- alpha(coul1,0.1)
      colors_in1_1 <- alpha(coul1,0.5)
      
      colors_in2 <- alpha(coul2,0.1)
      colors_in2_2 <- alpha(coul2,0.5)
      
      # dev.new()
      # #jpeg('sentiment_rplot2.png')
      # par(mar=rep(0.2,4))
      # par(mfrow=c(1,1))
      # 
      png(paste("rplot_Positive", Region, "2.png", sep="_")) 
      
      radarchart(UK_bing_combined_1, axistype=1, seg=3,
                 pcol=colors_border1, pfcol=colors_in1, plwd=2, plty=c(1,2),
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,40,8), cglwd=0.8,
                 #custom labels
                 vlcex=1.2,
                 title=Region)
      # legend(x=0.7, y=-.8, legend = rownames(UK_bing_combined_1[-c(1,2),]),
      #  pch=20 , col=colors_in1_1, lty=c(1,2),lwd=2, text.col = "black", cex=1.2, pt.cex=3)
      # 
      dev.off()#bty = "n",
      png(paste("rplot_Negative", Region, "2.png", sep="_")) 
      
      Sys.sleep(2)
      
      radarchart(UK_bing_combined_2, axistype=1, seg=3,
                 pcol=colors_border2, pfcol=colors_in2, plwd=2, plty=c(1,2),
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,60,12), cglwd=0.8,
                 #custom labels
                 vlcex=1.2,
                 title=Region)
      # legend(x=0.7, y=-.8, legend = rownames(UK_bing_combined_2[-c(1,2),]),
      #        pch=20 , col=colors_in2_2, lty=c(1,2),lwd=2, text.col = "black", cex=1.2, pt.cex=3)
      dev.off()
      
      Sys.sleep(2)
      
      #dev.off()
      
    }
    







#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#Emotion chart (to be corrected for negations)  ..often just look at words in isolation)
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
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
nrc_sent_Combn_2 <- NULL

for(i in 1:length(Pf_regions_uni)){ #i=8
  
  subsetP <- Pf_names_regions %>% 
    filter(Regions == Pf_regions_uni[i])
  
  dat2 <- NULL
  dat2_2 <- NULL
  
  for(j in 1:nrow(subsetP)) {#j=1
    
    dat2 <- rbind(dat2,
                  read.table(file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "police_cleaned_", subsetP$Police.Force[j],"_pandemic_nrc_2", ".csv", sep=""), sep=",", head=TRUE))
    
    dat2_2 <- rbind(dat2_2, 
                    read.table(file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "police_cleaned_", subsetP$Police.Force[j],"_non_p_nrc", ".csv", sep=""), sep=",", head=TRUE))
    
    # dat2 <- rbind(dat2,
    #               read.table(file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "police_cleaned_", subsetP$Police.Force[j],"_pandemic_nrc", ".csv", sep=""), sep=",", head=TRUE))
    # 
    # dat2_2 <- rbind(dat2_2, 
    #                 read.table(file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "police_cleaned_", subsetP$Police.Force[j],"_non_p_nrc", ".csv", sep=""), sep=",", head=TRUE))
    # 
    
    spike_words <- c("pandemic", "police", "policing",
                     "lockdown",
                     "corona",
                     "coronavirus",
                     "covid",
                     "covid-19",
                     "covid19",
                     "virus",
                     "quarantine")
    
    #combine all
    UK_nrc = data.frame(dat2[which(!dat2$word %in% spike_words),])
    #combine all
    UK_nrc_2 = data.frame(dat2_2[which(!dat2_2$word %in% spike_words),])
    
    
    UK_nrc <-  UK_nrc %>%
      filter(country != "NA" & !sentiment %in% c("positive", "negative")) %>%
      count(sentiment, country) %>%
      group_by(country, sentiment) %>%
      summarise(sentiment_sum = sum(n)) %>%
      ungroup()
    
    UK_nrc_2 <-  UK_nrc_2 %>%
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
    
    UK_nrc__2 = UK_nrc_2 %>% 
      group_by(country) %>%
      dplyr::mutate(total=sum(sentiment_sum))%>%
      mutate(pct=round((sentiment_sum/total)*100, digits=2))
    
    UK_nrc__2 = data.frame(dcast(UK_nrc__2, sentiment ~ country))
    
    # Set graphic colors
    
    UK_nrc_2 = UK_nrc_ %>% gather(Country, valname, -sentiment) %>% spread(sentiment, valname)
    UK_nrc_2_2 = UK_nrc__2 %>% gather(Country, valname, -sentiment) %>% spread(sentiment, valname)
    head(UK_nrc_2)
    head(UK_nrc_2_2)
    #sort as: 
    
    dev.new()
    dev.new()
    #jpeg('sentiment_rplot2.png')
    par(mar=rep(0.8,4))
    par(mfrow=c(ceiling(nrow(UK_nrc_2)/2),2))
    
    for(k in 1:nrow(UK_nrc_2)){ #k=1
      
      P.Force = UK_nrc_2$Country[k]
      
      type_1 <- UK_nrc_2[k,]
      type_1 = type_1 %>%
        mutate(Country = "pandemic")
      
      type_2 <- UK_nrc_2_2[k,]
      type_2 = type_2 %>%
        mutate(Country = "non-pandemic")
      
      comb_nrc <- rbind(type_1, type_2)
      comb_nrc = comb_nrc %>%
        dplyr::rename(Type=Country) 
      
      sentimentN <- colnames(comb_nrc)[2:length(comb_nrc)]
      reference = comb_nrc$Type
      comb_nrc <- comb_nrc[match(reference, comb_nrc$Type),]
      
      comb_nrc = comb_nrc %>% select(-Type)#%>%
      
      row.names(comb_nrc) <- reference
      max_min = rbind(rep(24,length(colnames(comb_nrc))), 
                      rep(0, length(colnames(comb_nrc))))
      rownames(max_min)<- 1:2
      colnames(max_min) <- colnames(comb_nrc)
      comb_nrc = data.frame(rbind(max_min,comb_nrc))
      
      library(RColorBrewer)
      #coul <- brewer.pal(5, "Set1")
      coul <- c("#E41A1C", "#377EB8")
      colors_border <- coul
      library(scales)
      colors_in <- alpha(coul,0.1)
      colors_in2 <- alpha(coul,0.5)
      
      radarchart(comb_nrc, axistype=1, seg=3,
                 pcol=colors_border, pfcol=colors_in, plwd=2, plty=1,
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,24,8), cglwd=0.8,
                 #custom labels
                 vlcex=0.8,
                 title=P.Force)
      #legend(x=0.7, y=1.3, legend = rownames(comb_nrc[-c(1,2),]), 
             #bty = "n", pch=20 , col=colors_in2, text.col = "black", cex=1.2, pt.cex=3)
      #dev.off()
      
    }
    
  }
  
}

#dev.copy(png,'myplot.png')


