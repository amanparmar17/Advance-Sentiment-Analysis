# ========using dplyr and tidytext and tidyverse================



# library(tidyverse)
# library(tidytext)
# library(tokenizers)
# library(stopwords)
# 
# file=read.csv("datasets/main_dataset.csv",header=TRUE)
# 
# print(nrow(file))
# print(length(file))
# 
# trim <- function (x) gsub("^\\s+|\\s+$", "", x)
# 
# for(i in 1:nrow(file))
# {
#   text=trim(file[i:i,2])
#   
#   text=toString(text)
#   x=tokenize_words(text)
#   
# }




# =====================using sentimentr package===================

library(sentimentr)
library(tokenizers)

file=read.csv("main_dataset.csv",header=TRUE)
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

for(i in 1:nrow(file))
{
  text=trim(file[i:i,2])
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
    confidence=100
  }
  
  dfrm <- data.frame(text,confidence,sentiment)
  write.table(dfrm,file="sentiment_sentimentr_pack.csv", append=TRUE,sep = "," ,col.names = FALSE) 
  
}

