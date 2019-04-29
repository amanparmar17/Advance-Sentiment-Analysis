setwd("/home/aman")
setwd("git_repos/Advance-Sentiment-Analysis/shiny/wordcloud/")

library(tm)       #for text mining
library(SnowballC)      #apply Porter's word stemming algorithm
library(RColorBrewer)      #Provides color schemes for maps 
library(wordcloud)        #for making wordclouds

#importing the dataset

dataset<-read.csv("sentiment.csv")


required<-dataset[[11]]        #tweets column

#corpus

dataset<-Corpus(VectorSource(required))

dataset<-tm_map(dataset, PlainTextDocument)
dataset<-tm_map(dataset,tolower)
dataset<-tm_map(dataset,removeNumbers)
dataset<-tm_map(dataset,removeWords,stopwords("english"))
dataset<-tm_map(dataset,removePunctuation)
dataset<-tm_map(dataset,stripWhitespace)
dataset<-tm_map(dataset,stemDocument)



####
dtm <- TermDocumentMatrix(dataset)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


