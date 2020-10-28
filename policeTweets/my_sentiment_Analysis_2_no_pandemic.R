
#to analyse tweet with about policing with no reference to pandemic.

#setwd("D:/IndirefTweets/scottishRef")
setwd("C:/Users/55131065/Desktop/downloadTweets/")

#read polie force location
location <- read.csv(file="PoliceForce_location.csv", sep=",", head=TRUE)
head(location)

table(location$policeForce)

#new
data1 = read.table(file="./only_policeTweet_set_28_.csv", sep=",", head=TRUE) 


data = data1 #, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11,  
             #data12, data13, data14, data15, data16) #, data17, data18, data19, data20,
#data21, data22, data23, data24, data25)

rm(data1) #, data2, data3, data4, data5, data6, data7, data8, data9, data10, 
   #data11, data12, data13, data14, data15, data16) #, data17, data18, data19, data20,
#data21, data22, data23, data24, data25)
#which(duplicated(data$status_id))
#which(duplicated(data$status_id))
data$location6[1:15]
head(data)

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
# data2 = data %>%
#   dplyr::arrange(status_id) %>%
#   dplyr::filter(!duplicated(status_id))%>%
#   dplyr::filter(!is.na(reply_to_status_id))%>% #removes replies
#   dplyr::filter(is_retweet==FALSE)%>% #remove retweets
#   dplyr::mutate(location1= gsub(",.*$", "", location)) %>% #add city name
#   dplyr::select(-c(location))%>%
#   dplyr::rename(location=location1)

#USE BOTH ORGANIC AND REPLIES
data3 = data %>%
  dplyr::arrange(status_id) %>%
  dplyr::filter(!duplicated(status_id))%>%
  #dplyr::filter(is.na(reply_to_status_id))%>% #removes replies
  dplyr::filter(is_retweet==FALSE)%>% #remove retweets
  dplyr::mutate(location1= gsub(",.*$", "", location)) %>% #add city name
  dplyr::select(-c(location))%>%
  dplyr::rename(location=location1)

head(data3)
nrow(data3)
data3[1:20,]


# data_1 <- left_join(data1, location, by = "location", keep=TRUE)
# data_2 <- left_join(data2, location, by = "location", keep=TRUE)
data_3 <- left_join(data3, location, by = "location", keep=TRUE)


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

data = data_3
nrow(data)

#filter data that contains any word relating to pandemic

data <- dplyr::filter(data, !grepl(c('pandemic'),text))
data <- dplyr::filter(data, !grepl(c('lockdown'),text))
data <- dplyr::filter(data, !grepl(c('corona'),text))
data <- dplyr::filter(data, !grepl(c('coronavirus'),text))
data <- dplyr::filter(data, !grepl(c('covid'),text))
data <- dplyr::filter(data, !grepl(c('covid-19'),text))
data <- dplyr::filter(data, !grepl(c('virus'),text))
data <- dplyr::filter(data, !grepl(c('quarantine'),text))

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
  write.table(placeTwt, file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "NO_PAN_cleaned_", Pf_names_regions_uni[i], "_raw", ".csv", sep=""),
              sep=",", row.names = F)
  
  
  
  flush.console()
  print(i)
  
  write.table(placeTwt_nrc, file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "NO_PAN_cleaned_", Pf_names_regions_uni[i], "_nrc", ".csv", sep=""),
              sep=",", row.names = F)

  write.table(placeTwt_bing, file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "NO_PAN_cleaned_", Pf_names_regions_uni[i], "_bing", ".csv", sep=""),
              sep=",", row.names = F)

  write.table(placeTwt_afinn, file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "NO_PAN_cleaned_", Pf_names_regions_uni[i], "_afinn", ".csv", sep=""),
              sep=",", row.names = F)
  
  
}


# 
# prince_bigrams <- prince_data %>%
#   unnest_tokens(bigram, lyrics, token = "ngrams", n = 2)
# 
# bigrams_separated <- prince_bigrams %>%
#   separate(bigram, c("word1", "word2"), sep = " ")
# 




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

for(i in 1:length(Pf_regions_uni)){ #i=1
  
  subsetP <- Pf_names_regions %>% 
    filter(Regions == Pf_regions_uni[i])
  
  dat2 <- NULL
  
  for(j in 1:nrow(subsetP)) {#j=1
    dat2 <- rbind(dat2, 
                  read.table(file = paste("C:/Users/55131065/Desktop/downloadTweets/outputs/", "NO_PAN_cleaned_", subsetP$Police.Force[j],"_nrc", ".csv", sep=""), sep=",", head=TRUE))
    
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
                   "virus",
                   "quarantine")
  
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
             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,24,8), cglwd=0.8,
             #custom labels
             vlcex=0.8,
             title=Pf_regions_uni[i])
  
  
}

#dev.copy(png,'myplot.png')






