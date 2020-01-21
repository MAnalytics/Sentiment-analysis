#Wordcloud2
#https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html


#old 
# meru = paste(MeruTweetsCleaned, collapse=" ")
# ola = paste(OlaTweetsCleaned, collapse=" ")
# tfs = paste(TaxiForSureTweetsCleaned, collapse=" ")
# uber = paste(UberTweetsCleaned, collapse=" ")

#new

#create the data..
eng <- englandTwt %>%
  unnest_tokens(word, text) 
wal <- walesTwt %>%
  unnest_tokens(word, text) 
nir <- NITwt %>%
  unnest_tokens(word, text) 
sco <- scotlandTwt %>%
  unnest_tokens(word, text) 

eng = paste(eng, collapse=" ") 
wal = paste(wal, collapse=" ") 
nir = paste(nir, collapse=" ") 
sco = paste(sco, collapse=" ") 


# eng = paste(englandTwt, collapse=" ") 
# wal = paste(walesTwt, collapse=" ")
# nir = paste(NITwt, collapse=" ")
# sco = paste(scotlandTwt, collapse=" ")


#wordcloud(c(letters, LETTERS, 0:9), seq(1, 1000, len = 62)) #library(tm)

#remove the remaining special characters
#function to remove special xters

x <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-=" #or whatever
eng <- str_replace_all(eng, "[[:punct:]]", " ")
wal <- str_replace_all(wal, "[[:punct:]]", " ")
nir <- str_replace_all(nir, "[[:punct:]]", " ")
sco <- str_replace_all(sco, "[[:punct:]]", " ")

#remove
eng = str_remove_all(eng, "[\n]")
eng = str_remove_all(eng, "[\"]")
wal = str_remove_all(wal, "[\n]")
wal = str_remove_all(wal, "[\"]")
nir = str_remove_all(nir, "[\n]")
nir = str_remove_all(nir, "[\"]")
sco = str_remove_all(sco, "[\n]")
sco = str_remove_all(sco, "[\"]")

#remove all 'within' number & #remove all characters starting with numbers
eng = tmp <- gsub("\\d+", "", eng)
eng = gsub("? [[:digit:]]*", " ", eng)
wal = tmp <- gsub("\\d+", "", wal)
wal = gsub("? [[:digit:]]*", " ", wal)
nir = tmp <- gsub("\\d+", "", nir)
nir = gsub("? [[:digit:]]*", " ", nir)
sco = tmp <- gsub("\\d+", "", sco)
sco = gsub("? [[:digit:]]*", " ", sco)


#all = removeWords(eng,stopwords("english"))

#change the order
all = c(sco, nir, wal, eng)

# remove stop-words
all = removeWords(all,stopwords("english"))

#head(all)

# create corpus
corpus = Corpus(VectorSource(all))

# create term-document matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)   #tdm[500:2030,]
head(tdm)

cloud_tdm1 <- cbind(word=as.vector(row.names(tdm)), freq1=as.numeric(tdm[,1])) 
row.names(cloud_tdm1) <- as.vector(row.names(tdm))
cloud_tdm2 <- cbind(word=as.vector(row.names(tdm)), freq1=as.numeric(tdm[,2])) 
row.names(cloud_tdm2) <- as.vector(row.names(tdm))
cloud_tdm3 <- cbind(word=as.vector(row.names(tdm)), freq1=as.numeric(tdm[,3])) 
row.names(cloud_tdm3) <- as.vector(row.names(tdm))
cloud_tdm4 <- cbind(word=as.vector(row.names(tdm)), freq1=as.numeric(tdm[,4])) 
row.names(cloud_tdm4) <- as.vector(row.names(tdm))


write.table(cloud_tdm1, file="cloud1.csv", sep=",", row.names = F)
write.table(cloud_tdm2, file="cloud2.csv", sep=",", row.names = F)
write.table(cloud_tdm3, file="cloud3.csv", sep=",", row.names = F)
write.table(cloud_tdm4, file="cloud4.csv", sep=",", row.names = F)

cloud_tdm1 <- read.table(file="cloud1.csv", sep=",", head = TRUE)
cloud_tdm1 <- cloud_tdm1[which(cloud_tdm1$word!="indyref"),]  #remove indiref
cloud_tdm1 <- cloud_tdm1[order(-cloud_tdm1$freq1),]
cloud_tdm1 <- cloud_tdm1[1:500,]

cloud_tdm2 <- read.table(file="cloud2.csv", sep=",", head = TRUE)
cloud_tdm2 <- cloud_tdm2[which(cloud_tdm2$word!="indyref"),]  #remove indiref
cloud_tdm2 <- cloud_tdm2[order(-cloud_tdm2$freq1),]
cloud_tdm2 <- cloud_tdm2[1:500,]


cloud_tdm3 <- read.table(file="cloud3.csv", sep=",", head = TRUE)
cloud_tdm3 <- cloud_tdm3[which(cloud_tdm3$word!="indyref"),]  #remove indiref
cloud_tdm3 <- cloud_tdm3[order(-cloud_tdm3$freq1),]
cloud_tdm3 <- cloud_tdm3[1:500,]


cloud_tdm4 <- read.table(file="cloud4.csv", sep=",", head = TRUE)
cloud_tdm4 <- cloud_tdm4[which(cloud_tdm4$word!="indyref"),]  #remove indiref
cloud_tdm4 <- cloud_tdm4[order(-cloud_tdm4$freq1),]
cloud_tdm4 <- cloud_tdm4[1:500,]

# install.packages("devtools")
# install.packages("wordcloud2")
# require(devtools)
# devtools::install_github("lchiffon/wordcloud2")
# library(wordcloud2)

wordcloud2(data.frame(cloud_tdm1), backgroundColor = "white")
wordcloud2(data.frame(cloud_tdm2), backgroundColor = "white")
wordcloud2(data.frame(cloud_tdm3), backgroundColor = "white")
wordcloud2(data.frame(cloud_tdm4), backgroundColor = "white")



head(cloud_tdm)



##colnames(tdm) = c("MeruCabs", "OlaCabs", "TaxiForSure", "UberIndia")

colnames(tdm) = c("Scotland", "N.Ireland", "Wales", "England")


#-----------------------------
#individual wordcloud
#------------------------------------------------------------------------------

mode(cloud_tdm)
#cloud_tdm <- as.matrix(cloud_tdm)

wordcloud(cloud_tdm, backgroundColor = "grey")



dim(demoFreq)


demoFreq[,2]+1






cloud_tdm[1,]

head(cloud_tdm)

library(wordcloud)
head(demoFreq)
mode(demoFreq)
#------------------------------------------------------------------------
head(scot_tdm)
dev.new()
# comparison cloud
comparison.cloud(cloud_tdm, random.order=FALSE, 
                 #colors = c("#00B2FF", "red", "#FF0099", "#6600CC"), 
		     colors = c("cadetblue", "chartreuse4", "brown", "#E69F00"),
                 title.size=0.1, max.words=2000, match.colors=TRUE, title.colors=NULL)
 
# # commonality cloud
commonality.cloud(tdm, random.order=FALSE,
                  colors = brewer.pal(8, "Dark"), max.words=2000, 
                  title.size=1.5)


wordcloud(cloud_tdm, random.order=FALSE, 
                 #colors = c("#00B2FF", "red", "#FF0099", "#6600CC"), 
                 colors = c("cadetblue", "chartreuse4", "brown", "#E69F00"),
                 title.size=0.1, max.words=2000, match.colors=TRUE, title.colors=NULL)


getwd()

dev.new()

# -- STEP 3 : make the graphics !
 
# Graph 1 : first top 500 discriminant words
png("#102_1_comparison_cloud_top_500_words.png", width = 480, height = 480)
comparison.cloud(tdm, max.words=2000, random.order=FALSE,c(4,0.4), title.size=1.4)
dev.off()
 
dev.new()
# Graph 2 : first top 2000 discriminant words
png("#102_1_comparison_cloud_top_2000_words.png", width = 480, height = 480)
comparison.cloud(tdm,max.words=500,random.order=FALSE,c(4,0.4), title.size=1.4)
#dev.off()
 
# Graph 3: commonality word cloud : first top 2000 common words across classes
png("#103_commonality_wordcloud.png", width = 480, height = 480)
commonality.cloud(tdm, max.words=2000, random.order=FALSE)
dev.off()


#Using wordcloud2 package
library(wordcloud2)

figPath = system.file("examples/t.png",package = "wordcloud2")

figPath = "/Users/monsu/Documents/GitHub/Sentiment-analysis/figures/scotland.png"

wordcloud2(demoFreq, figPath = figPath, size = 1.5,color = "skyblue")
letterCloud(demoFreq, word = "R", size = 2)

devtools::install_github("lchiffon/wordcloud2")

wordcloud2(head(most_words,100), figPath="/Users/username/Desktop/download.png" , size = .2, color = "skyblue")





