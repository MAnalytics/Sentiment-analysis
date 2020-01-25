#for sentiment analysis....
#has it's own cleaning function
.libPaths("C:/R/Rlib")
library(colormap)
library(fmsb)
library(tidyr)


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

setwd("D:/IndirefTweets/scottishRef")

#old
#load("taxiTweets.RData")
#head(Meru_tweets)
#new
#get the tweets from each country
#new
#get the tweets from each country
data1 = read.table(file="./scottishIndy_byCountry_1.csv", sep=",", head=TRUE) 
data2 = read.table(file="./scottishIndy_byCountry_2.csv", sep=",", head=TRUE) 
data3 = read.table(file="./scottishIndy_byCountry_3.csv", sep=",", head=TRUE) 
data4 = read.table(file="./scottishIndy_byCountry_4.csv", sep=",", head=TRUE) 
data5 = read.table(file="./scottishIndy_byCountry_5.csv", sep=",", head=TRUE) 
data6 = read.table(file="./scottishIndy_byCountry_6.csv", sep=",", head=TRUE) 
data7 = read.table(file="./scottishIndy_byCountry_7.csv", sep=",", head=TRUE) 
data8 = read.table(file="./scottishIndy_byCountry_8.csv", sep=",", head=TRUE) 
data9 = read.table(file="./scottishIndy_byCountry_9.csv", sep=",", head=TRUE) 
data10 = read.table(file="./scottishIndy_byCountry_10.csv", sep=",", head=TRUE) 
data11 = read.table(file="./scottishIndy_byCountry_11.csv", sep=",", head=TRUE)
data12 = read.table(file="./scottishIndy_byCountry_12.csv", sep=",", head=TRUE) 
data13 = read.table(file="./scottishIndy_byCountry_13.csv", sep=",", head=TRUE) 
data14 = read.table(file="./scottishIndy_byCountry_14.csv", sep=",", head=TRUE) 
data15 = read.table(file="./scottishIndy_byCountry_15.csv", sep=",", head=TRUE)  


data = rbind(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, 
		data11, data12, data13, data14, data15)

rm(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, 
 	data11, data12, data13, data14, data15)

#which(duplicated(data$status_id))


#remove duplicates, replies, and retweets
data = data %>%
  dplyr::arrange(status_id) %>%
  dplyr::filter(!duplicated(status_id))%>%
  dplyr::filter(is.na(reply_to_status_id))%>% #removes replies
  dplyr::filter(is_retweet==FALSE) #remove retweets
  

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
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9]", " ", x)
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



englandTwt$text <- sapply(englandTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
walesTwt$text <- sapply(walesTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
NITwt$text <- sapply(NITwt,function(row) iconv(row, "latin1", "ASCII", sub=""))
scotlandTwt$text <- sapply(scotlandTwt,function(row) iconv(row, "latin1", "ASCII", sub=""))


#create the data..
englandTwt_nrc <- englandTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="England")

englandTwt_bing <- englandTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% #join with the lexicon
  mutate(country="England")

walesTwt_nrc <- walesTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="Wales")

walesTwt_bing <- walesTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% #join with the lexicon
  mutate(country="Wales")

NITwt_nrc <- NITwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="Northern Ireland")

NITwt_bing <- NITwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% #join with the lexicon
  mutate(country="Northern Ireland")

scotlandTwt_nrc <- scotlandTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("nrc")) %>% #join with the lexicon
  mutate(country="Scotland")

scotlandTwt_bing <- scotlandTwt %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% #join with the lexicon
  mutate(country="Scotland")


#combine all
UK_nrc = data.frame(rbind(englandTwt_nrc, walesTwt_nrc, NITwt_nrc, scotlandTwt_nrc))

UK_bing = data.frame(rbind(englandTwt_bing, walesTwt_bing, NITwt_bing, scotlandTwt_bing))

unique(UK_bing$sentiment) 
head(UK_nrc)
head(UK_bing)

#Define some colors to use throughout
my_colors <- c("#E69F00", "chartreuse4", "brown", "cadetblue")

grid.col = c("England" = my_colors[1], "Wales" = my_colors[2], "Northern Ireland" = my_colors[3], 
             "Scotland" = my_colors[4], "positive" = "grey", 
             "negative" = "grey")

grid.col = c("England" = my_colors[1], "Wales" = my_colors[2], "Northern Ireland" = my_colors[3], 
             "Scotland" = my_colors[4], "positive" = "green", 
             "negative" = "red")


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
#circos.par(gap.after = c(rep(5, length(unique(UK_nrc[[1]])) - 1), 15,
                         #rep(5, length(unique(UK_nrc[[2]])) - 1), 15))
#chordDiagram(UK_nrc, grid.col = grid.col, transparency = .2,annotationTrackHeight = c(0.06, 0.06))
#title("Sentiment analysis")


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
#Stacked histogram
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


#Polarity chart
#---------------------------------------------------------------------

a = data.frame(x = c("fl", "kl", "po", "rt", "st"), a = runif(5), b = runif(5), c = runif(5), stringsAsFactors=FALSE)

reference = c("po", "rt", "fl", "st", "kl")
a[match(reference, a$x),]

#Emotion chart
#----------------------------------------------------------------------
# Create data: note in High school for Jonathan:
UK_nrc_2 = UK_nrc_ %>% gather(Country, valname, -sentiment) %>% spread(sentiment, valname)
#sort as: 
reference = c("Northern.Ireland", "Scotland", "Wales", "England")
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
colors_border=colormap(colormap=colormaps$viridis, nshades=4, alpha=1)
colors_in=colormap(colormap=colormaps$viridis, nshades=4, alpha=0.3)

# Split the screen in 6 parts
par(mar=rep(0.8,4))
par(mfrow=c(2,2))

# Loop for each plot
for(i in 1:4){ #i=4
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


#---------------------------------------------------------------------

library(radarchart)

labs <- c("Communicator", "Data Wangler", "Programmer",
          "Technologist",  "Modeller", "Visualizer")

scores <- list(
  "Rich" = c(9, 7, 4, 5, 3, 7),
  "Andy" = c(7, 6, 6, 2, 6, 9),
  "Aimee" = c(6, 5, 8, 4, 7, 6)
)

chartJSRadar(scores = scores, labs = labs, maxScale = 10)


































#UK_bing_ = data.frame(dcast(UK_bing_, sentiment ~ country))
#row.names(UK_bing_) <- c("negative","positive")
#UK_bing_ = UK_bing_ %>%
		#dplyr::select(-sentiment)
#Stacked barplot
#barplot(UK_bing_)


jus_ <- cluster_dep_
jus_1 <- jus_[1,3]
jus_3 <- jus_[3,3]
jus_[1,3] <- jus_3
jus_[3,3] <- jus_1

jus_4 <- jus_[4,3]
jus_6 <- jus_[6,3]
jus_[4,3] <- jus_6
jus_[6,3] <- jus_4

jus_7 <- jus_[7,3]
jus_9 <- jus_[9,3]
jus_[7,3] <- jus_9
jus_[9,3] <- jus_7

jus_10 <- jus_[10,3]
jus_12 <- jus_[12,3]
jus_[10,3] <- jus_12
jus_[12,3] <- jus_10

jus_13 <- jus_[13,3]
jus_15 <- jus_[15,3]
jus_[13,3] <- jus_15
jus_[15,3] <- jus_13

dev.new()

library(reshape2)
library(tidyr)

p <- ggplot(UK_bing_) + geom_bar(aes(x = country, y = pct, fill = sentiment), stat = "identity", alpha = 0.98) +
  theme_light() + scale_fill_manual(values = c("red","green")) +
  scale_x_discrete(expand = c(0.06,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Country", y = "Variance proportion", caption = "Figure 3: Assocation between deprivation and inequality")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y = element_text(size=10),
        plot.caption = element_text(size=7, hjust = 1),
        legend.background = element_rect(linetype = 1, size = 0.5, colour = 1)) +
        geom_text(data=UK_bing_, aes(x = country, y = pct, label = paste(pct, "%", sep="")),
          size = 3.5, position = position_stack(vjust = 0.5))


p <- ggplot(UK_nrc_) + geom_bar(aes(x = country, y = pct, fill = sentiment), stat = "identity", alpha = 0.98) +
  theme_light() + scale_fill_manual(values = c("red","green","yellow","purple","orange","blue", "black","magenta")) +
  scale_x_discrete(expand = c(0.06,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(x = "Country", y = "Variance proportion", caption = "Figure 3: Assocation between deprivation and inequality")  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10),
        axis.text.y = element_text(size=10),
        plot.caption = element_text(size=7, hjust = 1),
        legend.background = element_rect(linetype = 1, size = 0.5, colour = 1)) +
        geom_text(data=UK_nrc_, aes(x = country, y = pct, label = paste(pct, "%", sep="")),
          size = 3.5, position = position_stack(vjust = 0.5))


p







