setwd("git_repos/Advance-Sentiment-Analysis/")


# script for running the sentimentr package on all the 13600+rows of the refined_train_data_final.csv to calculate the sentiment val for each tweet


library(sentimentr)
library(tokenizers)

file=read.csv("scripts/covered/classifiers/covered_classifiers/decision_tree/dec_tree_train.csv",header=TRUE)

#dim(file)
#colnames(file)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

for(i in 1:nrow(file))
{
  text=trim(file[i:i,4])
  #for forming special tokens without #tags included
  tokens= tokenize_words(text)
  
  #forming a string of the tokkens so that the hashtags can also be used in calculating
  #sentiments as the # is removed from them
  
  # comb_text=paste(tokens,collapse=" ")
  # senti=sentiment(comb_text)
  
  
  
  # or to remove the hashtags from the start and the end of the string, so that they do not contribute 
  #in calculating the sentiment
  #why is it better
  # :
  #eg:
  # text= my mood is good #notgood
  # if only # removed as in the first case-------- sentiment is +
  # but there is an ambiguity with the statement as there is both good and notgood
  # to remove this ambiguity remove the whole #tag -- i.e, #notgood
  #text= my mood is good
  #sentiment= positive
  #no ambiguity remaining
  text=sub("\\s*\\B#\\w+(?:\\s*#\\w+)*\\s*$", "", text)
  senti=sentiment(text)
  
  #finding the sentiment of the sentence
  
  
  #proving the apt sentiment to the sentiment on the basis of the senti value returned
  #along with a proper confidence value (in %) 
  if(senti[1][[4]]<0)
  {
    sentiment="negative"
    confidence=senti[1][[4]]*100
  }
  
  if(senti[1][[4]]>0)
  {
    sentiment="positive"
    confidence=senti[1][[4]]*100
  }
  
  if(senti[1][[4]] == 0)
  {
    sentiment="neutral"
    confidence=0
  }
  
  airline_name=file[i:i,3]
  timezone=file[i:i,5]
  org_senti=file[i:i,2]
  
  dfrm <- data.frame(text,confidence,org_senti,airline_name,timezone)
  write.table(dfrm,file="dec_tree_final_train.csv", append=TRUE,sep = "," ,col.names = FALSE) 
  
}

