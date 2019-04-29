#importing the dataset
setwd("/home/aman")
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


#wssplot(KMeans,nc=30,seed=1234)


#visualising the results

# nc=6 as using the elbow method the appropriate number of clusters should be 6

base_kmeans<-kmeans(KMeans_3,26)
base_kmeans
base_kmeans$centers
base_kmeans$size



#visualising the clusters formed
library(cluster)
library(factoextra)

fviz_cluster(base_kmeans, data = KMeans_3,show.clust.cent = FALSE,labelsize = 1)


#finding the number of positive negative and neutral words in each cluster

d=base_kmeans$cluster
len=length(d)

#initialising all the variables to 0

clus1_pos=clus1_neg=clus1_neu=clus2_pos=clus2_neg=clus2_neu=clus3_pos=clus3_neg=clus3_neu=clus4_pos=clus4_neg=clus4_neu=clus5_pos=clus5_neg=clus5_neu=clus6_pos=clus6_neg=clus6_neu=clus7_pos=clus7_neg=clus7_neu=clus8_pos=clus8_neg=clus8_neu=clus9_pos=clus9_neg=clus9_neu=clus10_pos=clus10_neg=clus10_neu=clus11_pos=clus11_neg=clus11_neu=clus12_pos=clus12_neg=clus12_neu=clus13_pos=clus13_neg=clus13_neu=clus14_pos=clus14_neg=clus14_neu=clus15_pos=clus15_neg=clus15_neu=clus16_pos=clus16_neg=clus16_neu=clus17_pos=clus17_neg=clus17_neu=clus18_pos=clus18_neg=clus18_neu=clus19_pos=clus19_neg=clus19_neu=clus20_pos=clus20_neg=clus20_neu=clus21_pos=clus21_neg=clus21_neu=clus22_pos=clus22_neg=clus22_neu=clus23_pos=clus23_neg=clus23_neu=clus24_pos=clus24_neg=clus24_neu=clus25_pos=clus25_neg=clus25_neu=clus26_pos=clus26_neg=clus26_neu=0

count1=count2=count3=count4=count5=count6=count7=count8=count9=count10=count11=count12=count13=count14=count15=count16=count17=count18=count18=count19=count20=count21=count22=count23=count24=count25=count26=0
lat1 = lat2 = lat3 = lat4 = lat5 = lat6 =lat7=lat8=lat9=lat10=lat11=lat12=lat13=lat14=lat15=lat16=lat17=lat18=lat19=lat20=lat21=lat22=lat23=lat24=lat25=lat26=long1 = long2 = long3 = long4 = long5 = long6 =long7=long8=long9=long10=long11=long12=long13=long14=long15=long16=long17=long17=long18=long19=long20=long21=long22=long23=long24=long25=long26 =data.frame()

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
    count1 = count1 +1
    lat1 <- rbind(lat1,KMeans$lati[i])
    long1 <- rbind(long1,KMeans$longi[i])
    
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
    count2 = count2 +1
    lat2 <- rbind(lat2,KMeans$lati[i])
    long2 <- rbind(long2,KMeans$longi[i])
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
    count3 = count3 +1
    lat3 <- rbind(lat3,KMeans$lati[i])
    long3 <- rbind(long3,KMeans$longi[i])
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
    count4 = count4 +1
    lat4 <- rbind(lat4,KMeans$lati[i])
    long4 <- rbind(long4,KMeans$longi[i])
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
    count5 = count5 +1
    lat5 <- rbind(lat5,KMeans$lati[i])
    long5 <- rbind(long5,KMeans$longi[i])
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
    count6 = count6 +1
    lat6 <- rbind(lat6,KMeans$lati[i])
    long6 <- rbind(long6,KMeans$long[i])
  }
  #cluster 7
  if(d[i]==7)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus7_pos=clus7_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus7_neg=clus7_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus7_neu=clus7_neu+1;
    }
    count7 = count7 +1
    lat7 <- rbind(lat7,KMeans$lati[i])
    long7 <- rbind(long7,KMeans$longi[i])
    
  }
  #cluster 8
  if(d[i]==8)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus8_pos=clus8_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus8_neg=clus8_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus8_neu=clus8_neu+1;
    }
    count8 = count8 +1
    lat8 <- rbind(lat8,KMeans$lati[i])
    long8 <- rbind(long8,KMeans$longi[i])
    
  }
  #cluster 9
  if(d[i]==9)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus9_pos=clus9_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus9_neg=clus9_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus9_neu=clus9_neu+1;
    }
    count9 = count9 +1
    lat9 <- rbind(lat9,KMeans$lati[i])
    long9 <- rbind(long9,KMeans$longi[i])
    
  }
  #cluster 10
  if(d[i]==10)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus10_pos=clus10_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus10_neg=clus10_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus10_neu=clus10_neu+1;
    }
    count10 = count10 +1
    lat10 <- rbind(lat10,KMeans$lati[i])
    long10 <- rbind(long10,KMeans$longi[i])
    
  }
  #cluster 11
  if(d[i]==11)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus11_pos=clus11_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus11_neg=clus11_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus11_neu=clus11_neu+1;
    }
    count11 = count11 +1
    lat11 <- rbind(lat11,KMeans$lati[i])
    long11 <- rbind(long11,KMeans$longi[i])
    
  }
  #cluster 12
  if(d[i]==12)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus12_pos=clus12_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus12_neg=clus12_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus12_neu=clus12_neu+1;
    }
    count12 = count12 +1
    lat12 <- rbind(lat12,KMeans$lati[i])
    long12 <- rbind(long12,KMeans$longi[i])
    
  }
  #cluster 13
  if(d[i]==13)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus13_pos=clus13_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus13_neg=clus13_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus13_neu=clus13_neu+1;
    }
    count13 = count13 +1
    lat13 <- rbind(lat13,KMeans$lati[i])
    long13 <- rbind(long13,KMeans$longi[i])
    
  }
  #cluster 14
  if(d[i]==14)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus14_pos=clus14_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus14_neg=clus14_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus14_neu=clus14_neu+1;
    }
    count14 = count14 +1
    lat14 <- rbind(lat14,KMeans$lati[i])
    long14 <- rbind(long14,KMeans$longi[i])
    
  }
  #cluster 15
  if(d[i]==15)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus15_pos=clus15_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus15_neg=clus15_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus15_neu=clus15_neu+1;
    }
    count15 = count15 +1
    lat15 <- rbind(lat15,KMeans$lati[i])
    long15 <- rbind(long15,KMeans$longi[i])
    
  }
  #cluster 16
  if(d[i]==16)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus16_pos=clus16_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus16_neg=clus16_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus16_neu=clus16_neu+1;
    }
    count16 = count16 +1
    lat16 <- rbind(lat16,KMeans$lati[i])
    long16 <- rbind(long16,KMeans$longi[i])
    
  }
  #cluster 17
  if(d[i]==17)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus17_pos=clus17_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus17_neg=clus17_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus17_neu=clus17_neu+1;
    }
    count17 = count17 +1
    lat17 <- rbind(lat17,KMeans$lati[i])
    long17 <- rbind(long17,KMeans$longi[i])
    
  }
  #cluster 18
  if(d[i]==18)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus18_pos=clus18_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus18_neg=clus18_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus18_neu=clus18_neu+1;
    }
    count18 = count18 +1
    lat18 <- rbind(lat18,KMeans$lati[i])
    long18 <- rbind(long18,KMeans$longi[i])
    
  }
  #cluster 19
  if(d[i]==19)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus19_pos=clus19_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus19_neg=clus19_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus19_neu=clus19_neu+1;
    }
    count19 = count19 +1
    lat19 <- rbind(lat19,KMeans$lati[i])
    long19 <- rbind(long19,KMeans$longi[i])
    
  }
  #cluster 20
  if(d[i]==20)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus20_pos=clus20_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus20_neg=clus20_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus20_neu=clus20_neu+1;
    }
    count20 = count20 +1
    lat20 <- rbind(lat20,KMeans$lati[i])
    long20 <- rbind(long20,KMeans$longi[i])
  }
  #cluster 21
  if(d[i]==21)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus21_pos=clus21_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus21_neg=clus21_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus21_neu=clus21_neu+1;
    }
    count21 = count21 +1
    lat21 <- rbind(lat21,KMeans$lati[i])
    long21 <- rbind(long21,KMeans$longi[i])
  }
  #cluster 22
  if(d[i]==22)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus22_pos=clus22_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus22_neg=clus22_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus22_neu=clus22_neu+1;
    }
    count22 = count22 +1
    lat22 <- rbind(lat22,KMeans$lati[i])
    long22 <- rbind(long22,KMeans$longi[i])
  }
  #cluster 23
  if(d[i]==23)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus23_pos=clus23_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus23_neg=clus23_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus23_neu=clus23_neu+1;
    }
    count23 = count23 +1
    lat23 <- rbind(lat23,KMeans$lati[i])
    long23 <- rbind(long23,KMeans$longi[i])
  }  
  #cluster 24
  if(d[i]==24)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus24_pos=clus24_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus24_neg=clus24_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus24_neu=clus24_neu+1;
    }
    count24 = count24 +1
    lat24 <- rbind(lat24,KMeans$lati[i])
    long24 <- rbind(long24,KMeans$longi[i])
  }
  #cluster 25
  if(d[i]==25)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus25_pos=clus25_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus25_neg=clus25_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus25_neu=clus25_neu+1;
    }
    count25 = count25 +1
    lat25 <- rbind(lat25,KMeans$lati[i])
    long25 <- rbind(long25,KMeans$longi[i])
  }
  #cluster 26
  if(d[i]==26)
  {
    if(as.integer(as.character(new_sentiment[i]))==1)
    {
      clus26_pos=clus26_pos+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==2)
    {
      clus26_neg=clus26_neg+1;
    }
    if(as.integer(as.character(new_sentiment[i]))==3)
    {
      clus26_neu=clus26_neu+1;
    }
    count26 = count26 +1
    lat26 <- rbind(lat26,KMeans$lati[i])
    long26 <- rbind(long26,KMeans$longi[i])
  }
  
}
coordinates1=coordinates2=coordinates3=coordinates4=coordinates5=coordinates6=coordinates7=coordinates8=coordinates9=coordinates10=coordinates11=coordinates12=coordinates13=coordinates14=coordinates15=coordinates16=coordinates17=coordinates18=coordinates19=coordinates20=coordinates21=coordinates22=coordinates23=coordinates24=coordinates25=coordinates26=data.frame()
coordinates1 = cbind(lat1,long1)
coordinates2 = cbind(lat2,long2)
coordinates3 = cbind(lat3,long3)
coordinates4 = cbind(lat4,long4)
coordinates5 = cbind(lat5,long5)
coordinates6 = cbind(lat6,long6)
coordinates7 = cbind(lat7,long7)
coordinates8 = cbind(lat8,long8)
coordinates9 = cbind(lat9,long9)
coordinates10 = cbind(lat10,long10)
coordinates11= cbind(lat11,long11)
coordinates12 = cbind(lat12,long12)
coordinates13 = cbind(lat13,long13)
coordinates14 = cbind(lat14,long14)
coordinates15 = cbind(lat15,long15)
coordinates16 = cbind(lat16,long16)
coordinates17 = cbind(lat17,long17)
coordinates18 = cbind(lat18,long18)
coordinates19 = cbind(lat19,long19)
coordinates20 = cbind(lat20,long20)
coordinates21 = cbind(lat21,long21)
coordinates22 = cbind(lat22,long22)
coordinates23 = cbind(lat23,long23)
coordinates24 = cbind(lat24,long24)
coordinates25 = cbind(lat25,long25)
coordinates26 = cbind(lat26,long26)

library("maptools")
library("maps")

map("usa", fill = TRUE, col = "white", bg="lightblue",ylim = c(20,70), mar=c(0,0,0,0))
points(coordinates1[,2], coordinates1[,1], col="blue", pch=16  )
points(coordinates2[,2], coordinates2[,1], col="red", pch=16  )
points(coordinates3[,2], coordinates3[,1], col="green", pch=16  )
points(coordinates4[,2], coordinates4[,1], col="yellow", pch=16  )
points(coordinates5[,2], coordinates5[,1], col="black", pch=16  )
points(coordinates6[,2], coordinates6[,1], col="orange", pch=16  )
# points(coordinates7[,2], coordinates7[,1], col="blue", pch=16  )
# points(coordinates8[,2], coordinates8[,1], col="blue", pch=16  )
# points(coordinates9[,2], coordinates9[,1], col="blue", pch=16  )
# points(coordinates10[,2], coordinates10[,1], col="blue", pch=16  )
# points(coordinates11[,2], coordinates11[,1], col="blue", pch=16  )
# points(coordinates12[,2], coordinates12[,1], col="blue", pch=16  )
# points(coordinates13[,2], coordinates13[,1], col="blue", pch=16  )
# points(coordinates14[,2], coordinates14[,1], col="blue", pch=16  )
# points(coordinates15[,2], coordinates15[,1], col="blue", pch=16  )
# points(coordinates16[,2], coordinates16[,1], col="blue", pch=16  )
# points(coordinates17[,2], coordinates17[,1], col="blue", pch=16  )
# points(coordinates18[,2], coordinates18[,1], col="blue", pch=16  )
# points(coordinates19[,2], coordinates19[,1], col="blue", pch=16  )
# points(coordinates20[,2], coordinates20[,1], col="red", pch=16  )
# points(coordinates21[,2], coordinates21[,1], col="red", pch=16  )
# points(coordinates22[,2], coordinates22[,1], col="red", pch=16  )
# points(coordinates23[,2], coordinates23[,1], col="red", pch=16  )
# points(coordinates24[,2], coordinates24[,1], col="red", pch=16  )
# points(coordinates25[,2], coordinates25[,1], col="red", pch=16  )
# points(coordinates26[,2], coordinates26[,1], col="red", pch=16  )

#printing the number of positive negative and neutral vals in the clusters

cat("CLUSTER==1\n")
cat("positive: ",clus1_pos,"\n")
cat("negative: ",clus1_neg,"\n")
cat("neutral: ",clus1_neu,"\n")


cat("CLUSTER==2\n")
cat("positive: ",clus2_pos,"\n")
cat("negative: ",clus2_neg,"\n")
cat("neutral: ",clus2_neu,"\n")



cat("CLUSTER==3\n")
cat("positive: ",clus3_pos,"\n")
cat("negative: ",clus3_neg,"\n")
cat("neutral: ",clus3_neu,"\n")



cat("CLUSTER==4\n")
cat("positive: ",clus4_pos,"\n")
cat("negative: ",clus4_neg,"\n")
cat("neutral: ",clus4_neu,"\n")



cat("CLUSTER==5\n")
cat("positive: ",clus5_pos,"\n")
cat("negative: ",clus5_neg,"\n")
cat("neutral: ",clus5_neu,"\n")



cat("CLUSTER==6\n")
cat("positive: ",clus6_pos,"\n")
cat("negative: ",clus6_neg,"\n")
cat("neutral: ",clus6_neu,"\n")


# After printing we found that:
# cluster 1:=== total 51===================> all neutral sentiment
# cluster 2:=== total 71===================> all neutral sentiment
# cluster 3:=== total 286===================> all negative sentiment
# cluster 4:=== total 146===================> all positive sentiment
# cluster 5:=== total 14===================> 10 neutral, 3 negative,1 positive
# cluster 6:=== total 287===================> 284 negative, 3 positive


#to find the target area we have to calculate the mean latitude and longitude
