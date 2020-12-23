library(dplyr)
library(doParallel)
library(stringi)
library(tm)
library(slam)
library(textmineR)
library(NLP)
library(ggplot2)
library(wordcloud)
library(rJava)
library(RWekajars)
library(SnowballC)

library(RColorBrewer)
library(qdap)
library(RWeka)
library(openNLP)



blogs <- readLines("~/Desktop/final/en_US/en_US.blogs.txt", encoding="UTF-8", warn= TRUE, skipNul = TRUE)
news <- readLines("~/Desktop/final/en_US/en_US.news.txt", encoding="UTF-8", warn= TRUE, skipNul = TRUE)
twitter <- readLines("~/Desktop/final/en_US/en_US.twitter.txt", encoding="UTF-8", warn= TRUE, skipNul = TRUE)


set.seed(1234)
subTwitter <- sample(twitter, size = 5000, replace = TRUE)
subBlogs <- sample(blogs, size = 5000, replace = TRUE)
subNews <- sample(news, size = 5000, replace = TRUE)
sample <- c(subTwitter, subBlogs, subNews)
writeLines(sample, "~/Desktop/final/Sample.txt")

conn <- file("~/Desktop/final/Sample.txt")
corpus <- readLines(conn)
corpus <- Corpus(VectorSource(corpus))


########################################################################################
########################################################################################



conn <- file("~/Desktop/final/Sample.txt")
corpus <- readLines(conn)
corpus <- Corpus(VectorSource(corpus))


removeURL<-function(x) gsub("http[[:alnum:]]*","",x)
removeSign<-function(x) gsub("[[:punct:]]","",x)
removeNum<-function(x) gsub("[[:digit:]]","",x)
removeapo<-function(x) gsub("'","",x)
removeNonASCII<-function(x) iconv(x, "latin1", "ASCII", sub="")
removerepeat<- function(x) gsub("([[:alpha:]])\\1{2,}", "\\1\\1", x)
toLowerCase <- function(x) sapply(x,tolower)
removeSpace<-function(x) gsub("\\s+"," ",x)
removeTh<-function(x) gsub(" th", "",x)


corpus<-tm_map(corpus,content_transformer(removeapo))#remove apostrophe
corpus<-tm_map(corpus,content_transformer(removeNum))#remove numbers
corpus<-tm_map(corpus,content_transformer(removeURL)) #remove web url
corpus<-tm_map(corpus,content_transformer(removeSign)) #remove number and punctuation except apostrophe
corpus<-tm_map(corpus,content_transformer(removeNonASCII)) #remove non-ASCII
corpus<-tm_map(corpus,content_transformer(toLowerCase))# convert uppercase to lowercase
corpus<-tm_map(corpus,content_transformer(removerepeat))# remove repeated alphabets in a words
corpus<-tm_map(corpus,content_transformer(removeSpace)) #remove multiple space
corpus<-tm_map(corpus,removeWords,stopwords("english")) #remove common english words
corpus<-tm_map(corpus,content_transformer(removeTh)) #remove th from words



dtm<- TermDocumentMatrix(corpus)
wordMatrix = as.data.frame((as.matrix(  dtm )) ) 
v <- sort(rowSums(wordMatrix),decreasing=TRUE)
unigram <- data.frame(word = names(v),freq=v)

names(unigram) <- c("word1", "freq")
unigram$word1 <- as.character(unigram$word1)

write.csv(unigram[unigram$freq > 1,],"unigram.csv",row.names=F)
unigram <- read.csv("unigram.csv",stringsAsFactors = F)
saveRDS(unigram, file = "unigram.RData")

############


bigramer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
dtm2<-TermDocumentMatrix(corpus,control = list(tokenize = bigramer))
wordMatrix2 = as.data.frame((as.matrix(  dtm2 )) ) 
v2 <- sort(rowSums(wordMatrix2),decreasing=TRUE)
bigram<- data.frame(word = names(v2),freq=v2)

names(bigram) <- c("words","freq")

bigram$words <- as.character(bigram$words)
str2 <- strsplit(bigram$words,split=" ")
bigram <- transform(bigram, 
                    one = sapply(str2,"[[",1),   
                    two = sapply(str2,"[[",2))
bigram <- data.frame(word1 = bigram$one,word2 = bigram$two,freq = bigram$freq,stringsAsFactors=FALSE)

names(bigram)[names(bigram) == 'word1'] <- 'w1'
names(bigram)[names(bigram) == 'word2'] <- 'w2'

write.csv(bigram[bigram$freq > 2,],"bigram.csv",row.names=F)
bigram <- read.csv("bigram.csv",stringsAsFactors = F)
saveRDS(bigram,"bigram.RData")

###########


trigramer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
dtm3<-TermDocumentMatrix(corpus,control = list(tokenize = trigramer))
wordMatrix3 <- as.data.frame(as.matrix(dtm3))
v3 <- sort(rowSums(wordMatrix3),decreasing=TRUE)
trigram<- data.frame(word = names(v3),freq=v3)


names(trigram) <- c("words","freq")


trigram$words <- as.character(trigram$words)
str3 <- strsplit(trigram$words,split=" ")
trigram <- transform(trigram,
                     one = sapply(str3,"[[",1),
                     two = sapply(str3,"[[",2),
                     three = sapply(str3,"[[",3))

trigram <- data.frame(word1 = trigram$one,word2 = trigram$two, 
                      word3 = trigram$three, freq = trigram$freq,stringsAsFactors=FALSE)

names(trigram)[names(trigram) == 'word1'] <- 'w1'
names(trigram)[names(trigram) == 'word2'] <- 'w2'
names(trigram)[names(trigram) == 'word3'] <- 'w3'

write.csv(trigram[trigram$freq > 1,],"trigram.csv",row.names=F)
trigram <- read.csv("trigram.csv",stringsAsFactors = F)
saveRDS(trigram,"trigram.RData")


#############


quagramer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
dtm4<-TermDocumentMatrix(corpus,control = list(tokenize = quagramer))
wordMatrix4 <- as.data.frame(as.matrix(dtm4))
v4 <- sort(rowSums(wordMatrix4),decreasing=TRUE)
quadgram<- data.frame(word = names(v4),freq=v4)

names(quadgram) <- c("words","freq")


quadgram$words <- as.character(quadgram$words)

str4 <- strsplit(quadgram$words,split=" ")
quadgram <- transform(quadgram,
                      one = sapply(str4,"[[",1),
                      two = sapply(str4,"[[",2),
                      three = sapply(str4,"[[",3), 
                      four = sapply(str4,"[[",4))

quadgram <- data.frame(word1 = quadgram$one,
                       word2 = quadgram$two, 
                       word3 = quadgram$three, 
                       word4 = quadgram$four, 
                       freq = quadgram$freq, stringsAsFactors=FALSE)

names(quadgram)[names(quadgram) == 'word1'] <- 'w1'
names(quadgram)[names(quadgram) == 'word2'] <- 'w2'
names(quadgram)[names(quadgram) == 'word3'] <- 'w3'
names(quadgram)[names(quadgram) == 'word4'] <- 'w4'

write.csv(quadgram[quadgram$freq > 1,],"quadgram.csv",row.names=F)
quadgram <- read.csv("quadgram.csv",stringsAsFactors = F)
saveRDS(quadgram,"quadgram.RData")
