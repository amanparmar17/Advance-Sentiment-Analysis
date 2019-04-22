setwd("/home/aman")

setwd("git_repos/Advance-Sentiment-Analysis/scripts/classifiers/kmeans/")
file <- read.csv("refined_test_data.csv",header=TRUE)

coords=file$tweet_coord

for(i in 1:nrow(file))
{
  coord=file[i:i,8]
  #to remove the square brackets from the list
  coord=gsub("\\[|\\]","",coord)
  coords=(strsplit(as.character(coord),","))
  latitude=as.numeric(coords[[1]][1])
  longitude=as.numeric(coords[[1]][2])
  
  #removing all the rows with 0 in either latitude or longitude column
  if(latitude!=0 && longitude!=0)
  {
    tweet=file[i:i,7]
    sentiment=file[i:i,2]
    
    dfrm <- data.frame(tweet,sentiment,latitude,longitude)
    write.table(dfrm,file="kmeans_file.csv", append=TRUE,sep = "," ,col.names = FALSE) 
    
  }
  
  }




file2 <- read.csv("kmeans_file.csv",header=TRUE)
#total of 804 rows retreived

