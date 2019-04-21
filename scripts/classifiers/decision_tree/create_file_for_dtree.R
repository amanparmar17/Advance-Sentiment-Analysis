file= read.csv("scripts/classifiers/decision_tree/dec_tree_final_train.csv",header = TRUE)

# creating ranges

# [-100 => -50)          ==== range A
# [-50 => 0)          ==== range B
# 0          ==== range C
# (0 => 50)          ==== range D
# [50 => 100)          ==== range E

for(i in 1:11000)
{
  senti=as.numeric(as.character(file[i:i,2]))
  
  if(senti>=(-100) && senti<(-50))
    range="A"
  if(senti>=(-50) && senti<0)
    range="B"
  if(senti==0)
    range="C"
  if(senti>0 && senti<50)
    range="D"
  if(senti>=50 && senti<=100)
    range="E"
  
  tweet=file[i:i,1]
  sentiment_value=senti
  sentiment=file[i:i,3]
  airline=file[i:i,4]
  timezone=file[i:i,5]
  
  cols=c(tweet,sentiment_value,sentiment,range,airline,timezone)
  dfrm <- data.frame(tweet,sentiment_value,sentiment,range,airline,timezone)
  write.table(dfrm,file="scripts/classifiers/decision_tree/finale.csv", append=TRUE,sep = ",",col.names = FALSE)
}





#reading the file and removing the NA vals of the timezone or airline

file2= read.csv("scripts/classifiers/decision_tree/finale.csv",header = TRUE)


data=file2[complete.cases(file2),]

write.table(data,file="scripts/classifiers/decision_tree/Finale.csv", append=TRUE,sep = ",",col.names = FALSE)






