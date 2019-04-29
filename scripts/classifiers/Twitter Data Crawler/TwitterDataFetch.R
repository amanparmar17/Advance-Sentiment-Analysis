library(httr)
library(twitteR)

#Using OAuth authentication handshake functions from the httr package for a twitteR session
setup_twitter_oauth('4T4HL518fn0AFR1giSLKisY5U', 'GCBxvj0W3b7NomDauIMfbjx6VFhr45IIjTnp0JrO6nQN5TymwK', '4567773974-roQtQdQwg5hRO6Th5sVfOlMg5YqDCa0oJD1yMZL', 'vsQLqzMqbGyDi0yvoiUqJPwhdjPk9r41bYO1Sf8yNWqnh')

#Using function searchTwitter to search the given hashtag with given data entries and saving them into a variable
tweets <- searchTwitter('#demonetization', n=50) 

## all elements of tweets will be collapsed into
## a data.frame and returned into tweet.df
tweets.df <- do.call(rbind, lapply(tweets, as.data.frame))

#Writing the tweets data frame into a new file and naming it as Minor13.csv
write.csv(tweets.df,file = "Minor124.csv",row.names=FALSE)



















#post processing and display
setwd("git_repos/Advance-Sentiment-Analysis/scripts/covered/classifiers/covered_classifiers/Twitter Data Crawler/")
file=read.csv("Minor124.csv",header=TRUE)

#preprocessing of the formed dataset for further display of info to the user

final=file[c("id","created","text","retweetCount","longitude","latitude")]
final=final[final$retweetCount >0,]

rownumber=nrow(file)
colnumber=ncol(file)
# head(final)

write.csv(final,file = "final.csv",row.names=FALSE)

mapply(function(x) sum(is.na(x)), final)
mapply(function(x) sum(!is.na(x)), final)



sec <- final[complete.cases(final),]
write.csv(sec,file = "sec.csv",row.names=FALSE)
# head(sec)


# geoplotting of coordinates
fi=read.csv("sec.csv",header=TRUE)

# library(ggplot2)
# library(ggmap)

# creating a sample data.frame with your lat/lon points

df <- fi[c("latitude","longitude")]
colnames(df)=c("lat","lon")


library(maptools)
library(maps)

visit.x <- df$lon
visit.y <- df$lat
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
# points(visit.x,visit.y, col="red", pch=16)

for(i in 1:17)
{
  points(visit.x[i],visit.y[i], col="red", pch=16)
  print(i)
  Sys.sleep(3)
}


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


#analysing the 1