#Wordcloud2
#https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html


#old 
# meru = paste(MeruTweetsCleaned, collapse=" ")
# ola = paste(OlaTweetsCleaned, collapse=" ")
# tfs = paste(TaxiForSureTweetsCleaned, collapse=" ")
# uber = paste(UberTweetsCleaned, collapse=" ")

#new
eng = paste(englandTwtCleaned, collapse=" ") #wal =englandTwtCleaned;  nir=englandTwtCleaned; sco=englandTwtCleaned
#tweet = gsub("[ \t]{2,}", " ", tweet)
wal = paste(walesTwtCleaned, collapse=" ")
nir = paste(NITwtCleaned, collapse=" ")
sco = paste(scotlandTwtCleaned, collapse=" ")

#wordcloud(c(letters, LETTERS, 0:9), seq(1, 1000, len = 62))

# put everything in a single vector
#all = c(eng, wal, nir, sco) length(all)
all = c(sco, nir, wal, eng)

# remove stop-words
require(tm)
require(wordcloud)
all = removeWords(all,stopwords("english"))

#all
#rm(list=ls())
#old
###all = removeWords(all,c("ola", "code", "app", "download", "sign","earn", "olacabs", "referral"))
all = removeWords(all,c("otb", "fvf", "didnt", "also", "aaw", "gsym", "became", "will",
                        "ifuy","irate","bit","fat","will", "put", "qvo"))
all = removeWords(all,c("you", "your", "team", "address", "taxiforsure","taxi","for","sure"))
all = removeWords(all,c("blame", "news", "uber", "delhi"))
# create corpus
corpus = Corpus(VectorSource(all))

# create term-document matrix
tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)

head(tdm)
# add column names
##colnames(tdm) = c("MeruCabs", "OlaCabs", "TaxiForSure", "UberIndia")

#colnames(tdm) = c("England", "Wales", "N.Ireland", "Scotland")
colnames(tdm) = c("Scotland", "N.Ireland", "Wales", "England")

scot_tdm <- data.frame(cbind(word=row.names(tdm), freq=tdm[,4]))  #dim(scot_tdm)
colnames(scot_tdm) <- c("word", "freq")

head(scot_tdm)
dev.new()
# comparison cloud
comparison.cloud(tdm, random.order=FALSE, 
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC"), 
                 title.size=1.5, max.words=500, match.colors=FALSE)

# # commonality cloud
commonality.cloud(tdm, random.order=FALSE,
                  colors = brewer.pal(8, "Dark2"),
                  title.size=1.5)


