setwd("/home/aman")
setwd("git_repos/Advance-Sentiment-Analysis/shiny/kmeans/")

KMeans_3=read.csv("kmeans3.csv",header=TRUE)


base_kmeans<-kmeans(KMeans_3,26)
base_kmeans
base_kmeans$centers
base_kmeans$size


final_df=data.frame()
final_df=cbind(base_kmeans$centers,base_kmeans$size)

colnames(final_df)=(c('average_sentiment',"longitude","latitude",'element_count'))
final_df=as.data.frame(final_df)
count=sum(final_df$element_count)

#finding the number of positive negative and neutral words in each cluster

d=base_kmeans$cluster
len=length(d)

wss=base_kmeans$tot.withinss

#initialising all the variables to 0
# clus1_pos=clus1_neg=clus1_neu=clus2_pos=clus2_neg=clus2_neu=clus3_pos=clus3_neg=clus3_neu=clus4_pos=clus4_neg=clus4_neu=clus5_pos=clus5_neg=clus5_neu=clus6_pos=clus6_neg=clus6_neu=0
# 
# for(i in 1:len)
# {
#   #cluster 1
#   if(d[i]==1)
#   {
#     if(as.integer(as.character(new_sentiment[i]))==1)
#     {
#       clus1_pos=clus1_pos+1;
#     }
#     if(as.integer(as.character(new_sentiment[i]))==2)
#     {
#       clus1_neg=clus1_neg+1;
#     }
#     if(as.integer(as.character(new_sentiment[i]))==3)
#     {
#       clus1_neu=clus1_neu+1;
#     }
#   }
#   
#   #cluster 2
#   if(d[i]==2)
#   {
#     if(as.integer(as.character(new_sentiment[i]))==1)
#     {
#       clus2_pos=clus2_pos+1;
#     }
#     if(as.integer(as.character(new_sentiment[i]))==2)
#     {
#       clus2_neg=clus2_neg+1;
#     }
#     if(as.integer(as.character(new_sentiment[i]))==3)
#     {
#       clus2_neu=clus2_neu+1;
#     }
#   }
#   
#   #cluster 3
#   if(d[i]==3)
#   {
#     if(as.integer(as.character(new_sentiment[i]))==1)
#     {
#       clus3_pos=clus3_pos+1;
#     }
#     if(as.integer(as.character(new_sentiment[i]))==2)
#     {
#       clus3_neg=clus3_neg+1;
#     }
#     if(as.integer(as.character(new_sentiment[i]))==3)
#     {
#       clus3_neu=clus3_neu+1;
#     }
#   }
#   
#   #cluster 4
#   if(d[i]==4)
#   {
#     if(as.integer(as.character(new_sentiment[i]))==1)
#     {
#       clus4_pos=clus4_pos+1;
#     }
#     if(as.integer(as.character(new_sentiment[i]))==2)
#     {
#       clus4_neg=clus4_neg+1;
#     }
#     if(as.integer(as.character(new_sentiment[i]))==3)
#     {
#       clus4_neu=clus4_neu+1;
#     }
#   }
#   
#   #cluster 5
#   if(d[i]==5)
#   {
#     if(as.integer(as.character(new_sentiment[i]))==1)
#     {
#       clus5_pos=clus5_pos+1;
#     }
#     if(as.integer(as.character(new_sentiment[i]))==2)
#     {
#       clus5_neg=clus5_neg+1;
#     }
#     if(as.integer(as.character(new_sentiment[i]))==3)
#     {
#       clus5_neu=clus5_neu+1;
#     }
#   }
#   
#   #cluster 6
#   if(d[i]==6)
#   {
#     if(as.integer(as.character(new_sentiment[i]))==1)
#     {
#       clus6_pos=clus6_pos+1;
#     }
#     if(as.integer(as.character(new_sentiment[i]))==2)
#     {
#       clus6_neg=clus6_neg+1;
#     }
#     if(as.integer(as.character(new_sentiment[i]))==3)
#     {
#       clus6_neu=clus6_neu+1;
#     }
#   }
# }


info=data.frame()
o1=c(0,19,0,19)
o2=c(0,16,0,16)
o3=c(66,0,0,66)
o4=c(0,0,11,11)
o5=c(32,0,0,32)
o6=c(31,0,0,31)
o7=c(3,10,1,14)
o8=c(18,0,0,18)
o9=c(16,0,0,16)
o10=c(35,0,0,35)
o11=c(12,2,3,17)
o12=c(25,0,0,25)
o13=c(60,0,0,60)
o14=c(50,0,0,50)
o15=c(0,0,28,28)
o16=c(0,0,39,39)
o17=c(0,21,0,21)
o18=c(35,0,0,35)
o19=c(0,0,15,15)
o20=c(9,0,0,9)
o21=c(50,0,0,50)
o22=c(0,0,13,13)
o23=c(57,0,0,57)
o24=c(74,0,0,74)
o25=c(0,0,53,53)
o26=c(0,51,0,51)
info=rbind(o1,o2,o3,o4,o5,o6,o7,o8,o9,o10,o11,o12,o13,o14,o15,o16,o17,o18,o19,o20,o21,o22,o23,o24,o25,o26)
rownames(info)=c("cluster1","cluster2","cluster3","cluster4","cluster5","cluster6","cluster7","cluster8","cluster9",
                 "cluster10","cluster11","cluster12","cluster13","cluster14","cluster15","cluster16","cluster17","cluster18","cluster19",
                 "cluster20","cluster21","cluster22","cluster23","cluster24","cluster25","cluster26")
colnames(info)=c('negative','neutral','positive','total')

cluster=c("cluster1","cluster2","cluster3","cluster4","cluster5","cluster6","cluster7","cluster8","cluster9",
     "cluster10","cluster11","cluster12","cluster13","cluster14","cluster15","cluster16","cluster17","cluster18","cluster19",
     "cluster20","cluster21","cluster22","cluster23","cluster24","cluster25","cluster26")
info=cbind(cluster,info)



#printing the number of positive negative and neutral vals in the clusters
# 
# cat("CLUSTER==1\n")
# cat("positive: ",clus1_pos)
# cat("negative: ",clus1_neg)
# cat("neutral: ",clus1_neu)
# 
# 
# cat("CLUSTER==2\n")
# cat("positive: ",clus2_pos)
# cat("negative: ",clus2_neg)
# cat("neutral: ",clus2_neu)
# 
# 
# 
# cat("CLUSTER==3\n")
# cat("positive: ",clus3_pos)
# cat("negative: ",clus3_neg)
# cat("neutral: ",clus3_neu)
# 
# 
# 
# cat("CLUSTER==4\n")
# cat("positive: ",clus4_pos)
# cat("negative: ",clus4_neg)
# cat("neutral: ",clus4_neu)
# 
# 
# 
# cat("CLUSTER==5\n")
# cat("positive: ",clus5_pos)
# cat("negative: ",clus5_neg)
# cat("neutral: ",clus5_neu)
# 
# 
# 
# cat("CLUSTER==6\n")
# cat("positive: ",clus6_pos)
# cat("negative: ",clus6_neg)
# cat("neutral: ",clus6_neu)


# After printing we found that:
# 
# cluster 1:=== total 51===================> all neutral sentiment
# cluster 2:=== total 71===================> all neutral sentiment
# cluster 3:=== total 286===================> all negative sentiment
# cluster 4:=== total 146===================> all positive sentiment
# cluster 5:=== total 14===================> 10 neutral, 3 negative,1 positive
# cluster 6:=== total 287===================> 284 negative, 3 positive


#to find the target area we have to calculate the mean latitude and longitude





#shiny data
no_target_rows=nrow(KMeans_3)
method_used="Elbow method"
optimal_clusters=6

#image of elbow graph
#kmeans graph

#cluster info iin the form of a table cluster-number of values- sentiment analysis
# cluster 1:=== total 51===================> all neutral sentiment
# cluster 2:=== total 71===================> all neutral sentiment
# cluster 3:=== total 286===================> all negative sentiment
# cluster 4:=== total 146===================> all positive sentiment
# cluster 5:=== total 14===================> 10 neutral, 3 negative,1 positive
# cluster 6:=== total 287===================> 284 negative, 3 positive

# also add the mean lati, longi of each cluster

# avg sentiment of cluster
# c1-neutral
# c2-neutral
# c3-negative
# c4-positive
# c5-neutral
# c6-negative
