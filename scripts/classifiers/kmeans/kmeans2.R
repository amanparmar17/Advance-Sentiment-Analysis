#importing the dataset

setwd("git_repos/Advance-Sentiment-Analysis/scripts/classifiers/kmeans/")

dataset <- read.csv("kmeans_file.csv",header=TRUE)



#preprocessing the whole file-changing the categorical var sentiment to a number

new_sentiment = factor(dataset$sentiment, levels = c('positive','negative','neutral'), labels = c(1,2,3))


as.data.frame(new_sentiment)

dataset$category_sentiment=new_sentiment







#selecting the columns of concern  from the dataset in another variable

# %>% is called the pipe symbol

library(magrittr) # need to run every time you start R and want to use %>%
library(dplyr)   # alternative, this also loads %>%

preprocessed_data <- dataset %>%
  select(senti = category_sentiment, longi = longitude, lati = latitude)

#substituting null values with the mean values of the column

Mean_longi<-mean(preprocessed_data$longi, na.rm=TRUE)
Mean_lati<-mean(preprocessed_data$lati, na.rm=TRUE)
preprocessed_data$lati[which(is.na(preprocessed_data$lati))] <- Mean_lati
preprocessed_data$longi[which(is.na(preprocessed_data$longi))] <- Mean_longi

#creating a CSV file to store the columns of concern

write.csv(preprocessed_data,"preprocessed_dataset.csv",row.names = F)

#importing the refined dataset

KMeans<-read.csv("preprocessed_dataset.csv")
# KMeans_2<-KMeans[-1]
KMeans_3<-as.data.frame(scale(KMeans))

write.csv(KMeans_3,"kmeans3.csv",row.names = F)

KMeans_3

#finding the mean and standard deviation

# sapply(KMeans_2,mean)
# sapply(KMeans_2,sd)
# sapply(KMeans_3,mean)
# sapply(KMeans_3,sd)

#applying KMeans to the datasets

# NbClust package provides 30 indices for determining the number of clusters and proposes to user 
# the best clustering scheme from the different results obtained by varying all combinations of number of 
# nrow(data)-1)*sum(apply(data,2,var))

  # for (i in 2:nc){
  #   set.seed(seed)
  #   wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  # plot(1:nc, wss, type="b", xlab="Number of Clusters",
  #      ylab="Within groups sum of squares")

library(NbClust)

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

  
# nc <- NbClust(KMeans_3, min.nc=2, max.nc=15, method="kmeans")
# print(table(nc$Best.n[1,]))

#plotting the graph with the clusters

wssplot(KMeans_3,nc=30,seed=1234)


wssplot(KMeans,nc=30,seed=1234)


#visualising the results

# nc=6 as using the elbow method the appropriate number of clusters should be 6

base_kmeans<-kmeans(KMeans_3,6)
base_kmeans
base_kmeans$centers
base_kmeans$size



#visualising the clusters formed
library(cluster)
library(factoextra)

fviz_cluster(base_kmeans, data = KMeans_3)


#finding the number of positive negative and neutral words in each cluster

d=base_kmeans$cluster
len=length(d)

#initialising all the variables to 0
clus1_pos=clus1_neg=clus1_neu=clus2_pos=clus2_neg=clus2_neu=clus3_pos=clus3_neg=clus3_neu=clus4_pos=clus4_neg=clus4_neu=clus5_pos=clus5_neg=clus5_neu=clus6_pos=clus6_neg=clus6_neu=0

for(i in 1:len)
{
  #cluster 1
  if(d[i]==1)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
       {
         clus1_pos=clus1_pos+1;
       }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus1_neg=clus1_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus1_neu=clus1_neu+1;
    }
  }
  
  #cluster 2
  if(d[i]==2)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus2_pos=clus2_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus2_neg=clus2_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus2_neu=clus2_neu+1;
    }
  }
  
  #cluster 3
  if(d[i]==3)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus3_pos=clus3_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus3_neg=clus3_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus3_neu=clus3_neu+1;
    }
  }
  
  #cluster 4
  if(d[i]==4)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus4_pos=clus4_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus4_neg=clus4_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus4_neu=clus4_neu+1;
    }
  }
  
  #cluster 5
  if(d[i]==5)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus5_pos=clus5_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus5_neg=clus5_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus5_neu=clus5_neu+1;
    }
  }
  
  #cluster 6
  if(d[i]==6)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus6_pos=clus6_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus6_neg=clus6_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus6_neu=clus6_neu+1;
    }
  }
}


#printing the number of positive negative and neutral vals in the clusters

cat("CLUSTER==1\n")
cat("positive: ",clus1_pos)
cat("negative: ",clus1_neg)
cat("neutral: ",clus1_neu)


cat("CLUSTER==2\n")
cat("positive: ",clus2_pos)
cat("negative: ",clus2_neg)
cat("neutral: ",clus2_neu)



cat("CLUSTER==3\n")
cat("positive: ",clus3_pos)
cat("negative: ",clus3_neg)
cat("neutral: ",clus3_neu)



cat("CLUSTER==4\n")
cat("positive: ",clus4_pos)
cat("negative: ",clus4_neg)
cat("neutral: ",clus4_neu)



cat("CLUSTER==5\n")
cat("positive: ",clus5_pos)
cat("negative: ",clus5_neg)
cat("neutral: ",clus5_neu)



cat("CLUSTER==6\n")
cat("positive: ",clus6_pos)
cat("negative: ",clus6_neg)
cat("neutral: ",clus6_neu)


# After printing we found that:
# 
# cluster 1:=== total 51===================> all neutral sentiment
# cluster 2:=== total 71===================> all neutral sentiment
# cluster 3:=== total 286===================> all negative sentiment
# cluster 4:=== total 146===================> all positive sentiment
# cluster 5:=== total 14===================> 10 neutral, 3 negative,1 positive
# cluster 6:=== total 287===================> 284 negative, 3 positive


#to find the target area we have to calculate the mean latitude and longitude
