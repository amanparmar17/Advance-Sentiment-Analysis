# RANDOM FOREST

setwd("/home/aman")
setwd("git_repos/Advance-Sentiment-Analysis/scripts/classifiers/random_forest/")


library(caret)
library(rpart.plot)
library(knitr)
library(rpart)
library(randomForest)

dataset= read.csv("Ffinale.csv",header = TRUE)

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
uniqueAirline  <- unique(train[,'airline'])
# uniqueAirline  <- as.vector(data.frame(uniqueAirline))
uniqueTimezone <- unique(train[,'timezone'])
# uniqueTimezone <- as.vector(data.frame(uniqueTimezone))

write.csv(train,"rforest_training.csv",row.names = F)

test <- sec[-train_ind, ]

 for(i in 1:nrow(test)){
   if(!(test$airline[i] %in% uniqueAirline ) || !(test$timezone[i] %in% uniqueTimezone ) ){
     test <- test[-i,]
     # test$airline[i]
   }
 }


write.csv(test,"rforest_testing.csv",row.names = F)

#importing the training and test set data

# stringsAsFactors:  so as to prevent the strings to be treated as factor objects, as they are very hard to operate on


rforestTrainingSet<-read.csv("rforest_training.csv",stringsAsFactors = TRUE)
rforestTrainingSet <- rforestTrainingSet[, c("sentiment", 'range', 'airline', 'timezone')]
rforestTrainingSet <- rforestTrainingSet[complete.cases(rforestTrainingSet),]


rforestTestingSet <-read.csv("rforest_testing.csv",stringsAsFactors = TRUE)

# rforestTrainingSet$sentiment = as.data.frame(rforestTrainingSet$sentiment)
# rforestTrainingSet$sentiment = factor(rforestTrainingSet$sentiment)

model=randomForest(sentiment ~  airline + timezone , data = rforestTrainingSet, method = "class",ntree = 100, importance = TRUE)

model

levels(rforestTestingSet$timezone) <- levels(rforestTrainingSet$timezone)
levels(rforestTestingSet$range) <- levels(rforestTrainingSet$range)
levels(rforestTestingSet$airline) <- levels(rforestTrainingSet$airline)

# Predicting on train set
predTrain= predict(model,rforestTrainingSet,type = "class")
# predTrain

table(predTrain, rforestTrainingSet$sentiment)

# Predicting on test set

predicted= predict(model,rforestTestingSet,type = "class")
# predicted

mean(predicted == rforestTestingSet$sentiment)
table(predicted , rforestTestingSet$sentiment)

# To check important variables
importance(model)        
varImpPlot(model)  
plot(model)





#manipulating the original data
original_class=as.factor(rforestTestingSet$sentiment)

results <- confusionMatrix(data=as.factor(predicted), reference=original_class)
print(results)

#dumping the model for future ref
saveRDS(model, file = "model.rds")

#loading the model again
model <- readRDS("forest.rds")



