# decision tree classifier on the train data

#new has been changes to section

setwd("git_repos/Advance-Sentiment-Analysis/")

library(caret)
library(rpart.plot)
library(knitr)
library(rpart)


dataset= read.csv("scripts/classifiers/decision_tree/Finale.csv",header = TRUE)

# summary(dataset)


#as the model takes a forever to train---- 9+hrs and still running
# reduce the number of rows to a randomly generated number 

# number=runif(1,as.integer((0.25*nrow(dataset))),as.integer((0.75*nrow(dataset))))
# section=dataset[1:number, c("sentiment", 'sentiment_value', 'airline', 'timezone')]


#creating a new custom dataset
section=dataset[, c("sentiment", 'range', 'airline', 'timezone')]

#verify the number and names of cols and the number of rows
print(colnames(section))
print(nrow(section))
print(ncol(section))

#verifying the sequence of the cols of the section dataframe
print(section[1:1,])

#since the number of rows is taking a forever so reducing the number of rows

# number=runif(1,as.integer(0.25*(nrow(dataset))),as.integer(0.75*(nrow(dataset))))
section=dataset[1:800, c("sentiment", 'range', 'airline', 'timezone')]
newsec=section[complete.cases(section),]


#verifying the existence of null values--------- returns the count of null and not null cells
mapply(function(x) sum(is.na(x)), newsec)
mapply(function(x) sum(!is.na(x)), newsec)

#getting only the not null fields-------------- returns 9036 rows
sec <- newsec[complete.cases(newsec),]

#setting up the sample size and seed 

smp_size <- floor(0.75 * nrow(sec))
set.seed(2)

#creating sepatate CSV files for training and test set datasets

train_ind <- sample(seq_len(nrow(sec)), size = smp_size)
train <- sec[train_ind, ]
write.csv(train,"scripts/classifiers/decision_tree/dtree_training.csv",row.names = F)

test <- sec[-train_ind, ]
write.csv(test,"scripts/classifiers/decision_tree/dtree_testing.csv",row.names = F)

#importing the training and test set data

# stringsAsFactors:  so as to prevent the strings to be treated as factor objects, as they are very hard to operate on


dtreeTrainingSet<-read.csv("scripts/classifiers/decision_tree/dtree_training.csv",stringsAsFactors = FALSE)
dtreeTrainingSet <- dtreeTrainingSet[, c("sentiment", 'range', 'airline', 'timezone')]
dtreeTrainingSet <- dtreeTrainingSet[complete.cases(dtreeTrainingSet),]

dtreeTestingSet <-read.csv("scripts/classifiers/decision_tree/dtree_testing.csv",stringsAsFactors = FALSE)

#fitting the decision tree classifier

fit <- rpart(sentiment ~ range + airline + timezone , data = dtreeTrainingSet, method = "class")

print(fit)

#
plot(fit)
text(fit)
set.seed(3)
rpart.plot(fit)
# 
# #


predicted= predict(fit,dtreeTestingSet)
predicted



