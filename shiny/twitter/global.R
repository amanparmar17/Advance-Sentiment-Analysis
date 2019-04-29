setwd("/home/aman")
setwd("git_repos/Advance-Sentiment-Analysis/shiny/twitter/")
file=read.csv("Minor124.csv",header=TRUE)

#preprocessing of the formed dataset for further display of info to the user

final=file[c("id","created","text","retweetCount","longitude","latitude")]
final=final[final$retweetCount >0,]



sec <- final[complete.cases(final),]


secc=sec[1:5,]
#%%
#extractin the tweet with the max retweet count
max_count=final[which.max(final$retweetCount),]
#printing the info of this tweet
tweet=max_count$text

creating =as.character(max_count$created)
creating=strsplit(creating," ")
create_date= creating[[1]][1]
create_time= creating[[1]][2]



retweet_count=max_count$retweetCount

library(sentimentr)
senti_analysis=sentiment(as.character(max_count$text))
sentival=mean(senti_analysis$sentiment)
if(sentival>0)
  sentiment="positive"
if(sentival<0)
  sentiment="negative"
if(sentival==0)
  sentiment="neutral"


d=cbind(as.character(tweet),create_date,create_time,retweet_count,sentival,sentiment)

d=as.data.frame(d)
#analysing the plotted tweets and geoplotting


fi=sec

df <- fi[c("text","retweetCount","created","latitude","longitude")]
# colnames(df)=c("lat","lon")

# visit.x <- df$lon
# visit.y <- df$lat

# dd=cbind(visit.x,visit.y)
dd=df[-c(7,8,9,10,13,17),]

library(sentimentr)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
data=data.frame()
for(i in 1:nrow(dd))
{
  text=trim(dd[i:i,1])
  text=sub("\\s*\\B#\\w+(?:\\s*#\\w+)*\\s*$", "", text)
  senti=sentiment(text)
  if(senti[1][[4]]<0)
  {
    sentiment="negative"
    confidence=senti[1][[4]]
  }
  
  if(senti[1][[4]]>0)
  {
    sentiment="positive"
    confidence=senti[1][[4]]
  }
  
  if(senti[1][[4]] == 0)
  {
    sentiment="neutral"
    confidence=0
  }
  
  retweet=as.character(dd$retweetCount[i])
  create=as.character(dd$created[i])
  lat=dd$latitude[i]
  lon=dd$longitude[i]
  
  # dfrm <- data.frame(text,confidence,sentiment,retweet,create,lat,lon)
  dfdf=cbind(text,confidence,sentiment,retweet,create,lat,lon)
  
  data=rbind(data,dfdf)
}



