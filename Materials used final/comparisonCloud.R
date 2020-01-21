#Wordcloud2
#https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html


#old 
# meru = paste(MeruTweetsCleaned, collapse=" ")
# ola = paste(OlaTweetsCleaned, collapse=" ")
# tfs = paste(TaxiForSureTweetsCleaned, collapse=" ")
# uber = paste(UberTweetsCleaned, collapse=" ")

#new
eng = paste(englandTwt, collapse=" ") 
wal = paste(walesTwt, collapse=" ")
nir = paste(NITwt, collapse=" ")
sco = paste(scotlandTwt, collapse=" ")


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

#> [1] "n ppl"    "tw prs"   "thr bnns"

# removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# # remove special characters
# 
# englandTwt$text <- sapply(englandTwt$text, removeSpecialChars)
# walesTwt$text <- sapply(walesTwt$text, removeSpecialChars)
# NITwt$text <- sapply(NITwt$text, removeSpecialChars)
# scotlandTwt$text <- sapply(scotlandTwt$text, removeSpecialChars)






#change the order
all = c(sco, nir, wal, eng)

# remove stop-words
all = removeWords(all,stopwords("english"))

head(all)

# create corpus
corpus = Corpus(VectorSource(all))

# create term-document matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)

head(tdm)
# add column names
##colnames(tdm) = c("MeruCabs", "OlaCabs", "TaxiForSure", "UberIndia")

colnames(tdm) = c("Scotland", "N.Ireland", "Wales", "England")

install.packages("devtools")
install.packages("wordcloud2")
require(devtools)
devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)
#-----------------------------
#individual wordcloud
#------------------------------------------------------------------------------

head(cloud_tdm)


cloud_tdm <- data.frame(cbind(word=row.names(tdm), freq=tdm[,1]))  #dim(scot_tdm)
colnames(cloud_tdm) <- c("word", "freq")

wordcloud2(cloud_tdm, backgroundColor = "grey")




#------------------------------------------------------------------------
head(scot_tdm)
dev.new()
# comparison cloud
comparison.cloud(tdm, random.order=FALSE, 
                 #colors = c("#00B2FF", "red", "#FF0099", "#6600CC"), 
		     colors = c("cadetblue", "chartreuse4", "brown", "#E69F00"),
                 title.size=0.1, max.words=2000, match.colors=TRUE, title.colors=NULL)
 
# # commonality cloud
commonality.cloud(tdm, random.order=FALSE,
                  colors = brewer.pal(8, "Dark"), max.words=2000, 
                  title.size=1.5)




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





