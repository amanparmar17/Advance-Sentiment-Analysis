#importing the libraries

setwd("/home/aman")
setwd("git_repos/Advance-Sentiment-Analysis/scripts/classifiers/WordCloud/")

library(tm)       #for text mining
library(SnowballC)      #apply Porter's word stemming algorithm
library(RColorBrewer)      #Provides color schemes for maps 
library(wordcloud)        #for making wordclouds

#importing the dataset

dataset<-read.csv("sentiment.csv")

#selecting the column of concern

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

#generating the WordCloud

wordcloud(words = dataset, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=0.65, 
          colors=brewer.pal(8, "Dark2"))

wordcloud(dataset,max.words = 200,random.color = TRUE,random.order=FALSE)



####
dtm <- TermDocumentMatrix(dataset)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


#

barplot(d[1:30,]$freq, las = 2, names.arg = d[1:30,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
