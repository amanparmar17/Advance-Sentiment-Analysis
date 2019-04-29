setwd("/home/aman")
setwd("shiny/final_ensemble/")

library(dplyr)
library(plyr)
library(e1071)

library(RTextTools)
library(e1071)
library(dplyr) 
library(tidytext) 
library(class) 
library(caret)

classifier_dec=readRDS('decision.rds')
classifier_rforest=readRDS('forest.rds')
classifier_nbayes=readRDS('nbayes.rds')


# file=read.csv("ensemblee_final.csv",header = TRUE)

uniqueTimezone=c('Pacific Time (US & Canada)' ,
                 'Central Time (US & Canada)',
                  'Eastern Time (US & Canada)',
                 'Jerusalem',                  
                  'Atlantic Time (Canada)',
                 'Quito',                      
                  'Arizona',
                 'Mountain Time (US & Canada)',
                  'Alaska'                  ,    'America/New_York'  ,         
                  'Hawaii'                    ,  'Amsterdam'          ,        
                  'London'                    ,  'Santiago'            ,       
                  'Sydney'                  ,    'Abu Dhabi'            ,      
                  'Guam'                      ,  'Caracas'               ,     
                  'Madrid'                     , 'Central America'        ,    
                  'Berlin'                    ,  'Edinburgh'               ,   
                  'Irkutsk'                   ,  'Kuala Lumpur'             ,  
                  'Taipei'                    ,  'America/Chicago'           , 
                  'Tijuana'                   ,  'Vienna'                     ,
                  'Brussels'           )