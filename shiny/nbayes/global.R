setwd("/home/aman")
setwd("git_repos/Advance-Sentiment-Analysis/shiny/nbayes/")

library(RTextTools)
library(e1071)
library(dplyr) 
library(tidytext) 
library(class) 
library(caret)  #for finding the confusion matrix


#importing the dataset

dataset<-read.csv("sentiment_sentimentr_pack.csv",header = TRUE,strip.white = TRUE)


responses<-dataset[[3]]
tweet_text<-dataset[[1]]

#removing the special characters from the selected column

removeSpecialChars <- function(tweet_text) gsub("[^a-zA-Z0-9 ]", " ", tweet_text)
tweet_text <- sapply(tweet_text, removeSpecialChars)

#converting the text data into lower case

tweet_text<-sapply(tweet_text,tolower)

#substitution of several words

fix.contractions<-function(tweet_text)
{
  tweet_text <- gsub("won't", "will not", tweet_text)
  tweet_text <- gsub("can't", "can not", tweet_text)
  tweet_text <- gsub("n't", " not", tweet_text)
  tweet_text <- gsub("'ll", " will", tweet_text)
  tweet_text <- gsub("'re", " are", tweet_text)
  tweet_text <- gsub("'ve", " have", tweet_text)
  tweet_text <- gsub("'m", " am", tweet_text)
  tweet_text <- gsub("'d", " would", tweet_text)
  tweet_text <- gsub("'s", "", tweet_text)
  return(tweet_text)}

#updating the imported dataset with the changes made in the column selected  

dataset$text<-tweet_text

#selecting the columns of concern  from the dataset in another variable

datasetrefined <- dataset %>% select(Response = sentiment,tweets = text)
# datasetrefined

application=datasetrefined


set.seed(123)
smp_size <- floor(0.75 * nrow(application))

#preparing the training set and storing the training set data in a separate CSV file

train_ind <- sample(seq_len(nrow(application)), size = smp_size)
train <- application[train_ind, ]
test <- application[-train_ind, ]


mat_train= create_matrix(train$tweets, language="english", 
                         removeStopwords=FALSE, removeNumbers=FALSE, 
                         stemWords=FALSE) 
matrix_train = as.matrix(mat_train)


mat_test= create_matrix(test$Response, language="english", 
                        removeStopwords=TRUE, removeNumbers=FALSE, 
                        stemWords=FALSE) 
matrix_test = as.matrix(mat_test)


classifier <- readRDS('model.rds')


predicted = predict(classifier,matrix_test[1:700,])
#it is the tabale containg the predicted values
predicted




#generating results in tabular form

predicted_table<-table(test[1:700,1], predicted)
#confusion matrix
predicted_table

#calculating the accuracy

predicted_accuracy<-recall_accuracy(test[1:700,1], predicted)
predicted_accuracy

predicted_data=as.factor(predicted)
original_class=as.factor(rownames(matrix_test[1:700,]))

results <- confusionMatrix(data=predicted_data, reference=original_class)
print(results)

