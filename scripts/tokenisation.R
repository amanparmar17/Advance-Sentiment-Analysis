# library(tidyverse)
# library(tidytext)
# library(glue)
# library(stringr)



# james="on looking there i found that aman was running very fat"
# #here james contains the sample string
# 
# #form tokens of the string
# tokenize_words(james)
# 
# #used for stemming ------- conversion of the words to their most basic form
# tokenize_word_stems(james)


#to remove stopwords
# tokenize_words(james, stopwords = stopwords::stopwords("en"))
# 
# 
# # to tokenise the tweets---------- as they contians special chars like @, # more often
# tokenize_tweets(james)


# looping through all the lines of the text column od the datset


# =================== file contains the final string formed by making stemmed tokens and then combining them===================

library(tokenizers)
library(stopwords)

f=read.csv("datasets/main_dataset.csv",header=TRUE)

# function to trim the leading ang trailing whitespaces from a string
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


for(i in 1:nrow(f))
{
  text=f[i:i,2]
  # special tokenisation of tweets, x returned here is a list
  text=toString(text)
  x=tokenize_tweets(text)
  # number of tokens such formed
  no_tokens=length(x[[1]])
  # forming a string out of these tokens so as to again apply stemming and remove stopwords
  proper_tweet= paste(x[[1]], collapse=" ")
  
  #removing the stopwords and forming the string again for future storage, z is a string
  y=tokenize_words(proper_tweet, stopwords = stopwords::stopwords("en"))
  proper_tweet=paste(y[[1]], collapse=" ")
  
  #applying stemming forming a string of the stemed tokens, y here is a list
  z=tokenize_word_stems(proper_tweet)
  stemmed_tweet= paste(y[[1]], collapse=" ")
  
  no_of_tokens=length(z[[1]])
  
  #forming the final processes tweet
  final_tweet=paste(z[[1]], collapse=" ")
  
  text=trim(text)
  final=trim(final_tweet)
  
  dfrm <- data.frame(text,final)
  write.table(dfrm,file="latest.csv", append=TRUE, sep=" , ", col.names = FALSE)  
  
  # cc=c(text,final_tweet)
  # write(cc,file="latest.csv", append=TRUE, sep=",")
}









#to add the number of tokens in the final string to the dataset







