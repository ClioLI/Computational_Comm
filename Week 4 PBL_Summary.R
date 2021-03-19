rm(list = ls()) #initialization

######################### Research proposal #########################
#########################1. Theoretical anchoring, including core concepts

####Concept Explication: 
####The process by which abstract concepts are systematically linked to observed variations in those concepts in the real world

####Conceptual definitions
#####Essential properties the researcher intends to be included within the concepts meaning

####Operational definitions
#####Procedures by which the concept is to be observed, measured, or manipulated

#example
#1. Concept: "Exposure to Televised News"

#2. Definition: the amount of time spent watching televised news programs

#3. Indicators:
#a. frequency of watching morning news
#b. frequency of watching national news at 5:30 p.m.
#c. frequency of watching local news
#d. frequency of watching television news magazine & interview programs

#4. Index: 
#Design an eleven-point scale, where zero means "never watch at all," one means "rarely watch" and ten "watch all the time."
#Apply the eleven-point scale to each of the four indicators by asking people to indicate how often they watch each of the above TV news programs.

#########################2. Work plan, including methods and analytic strategy 

#Experiments
#Field (or ???natural???) experiments
#Surveys
#Social network analysis
#Content analysis 
#Focused interviews
#Qualitative Field Research 
#...
#####Delineation of strengths and weaknesses of methodology, analytic approach!!!!!!

#Where is the computaitonal part in your proposal?
####Data collection/analysis

#########################3. Study justification and significance 
#Importance of research question 
#Originality of work and coverage of literature


######################### Programming Basics #########################
#see http://www.programmingbasics.org/en/

#Important Tips:
#1. Programming style: neatness, simplicity, meaningful names, comments, indentations and spaces
#2. Data structure and type
#3. Control flow and block diagram/flow chart
#4. Debugging (troubleshooting): always check the values and structures of the variables
#5. Scope of variable

#seting working directory
setwd("~/Documents/Teaching/PalmDrive/1.25.2021 PBL/PBL_ppt")
######################### Twitter API #########################

#install package
install.packages('rtweet')
if(!require("rtweet"))install.packages('rtweet')
if(!require("tm"))install.packages('tm')
if(!require("igraph"))install.packages('igraph')

library('rtweet')
library('tm')
library('igraph')

my_app <- "paste your app name here"
my_consumer_key <- "paste your consumer key here"
my_consumer_secret <- "paste consumer secret here"
my_token <- "paste your token here"
my_secret <- "paste your secret here"

#create token to get access to Twitter API
?create_token
token <- create_token(app = my_app,
                      consumer_key = my_consumer_key,
                      consumer_secret = my_consumer_secret,
                      access_token = my_token,
                      access_secret = my_secret,
                      set_renv = TRUE)

#search Twitter by search query (keywords): search_tweet() function
help(search_tweet) #check the document of this function and its parameter

Tweets_data <- search_tweets("#COVID-19", n = 10000, include_rts = TRUE, lang = "en")

data.frame(Tweets_data)

write_as_csv(Tweets_data, "~/Documents/Teaching/PalmDrive/1.25.2021 PBL/PBL_ppt/#COVID-19_data.csv",
             prepend=TRUE, na = "", fileEncoding = "UTF-8" )

######################### Data Exploration #########################
if(!require("tidyverse"))install.packages('tidyverse')

library('ggplot2')

# Exploration 1 : trend of discussion
ts_plot(Tweets_data, by = "30 sec") #plot the frequency of tweet over a specified interval of time

help(ts_plot)

#Exploration 2 : high frequency word
mt.v <- VectorSource(Tweets_data$text)
mt.c <- SimpleCorpus(mt.v)

inspect(mt.c)

mt.c.p <- tm_map(mt.c, content_transformer(tolower))
mt.c.p <- tm_map(mt.c, removeNumbers)
mt.c.p <- tm_map(mt.c, removeWords, stopwords('english'))
mt.c.p <- tm_map(mt.c, removePunctuation)
mt.c.p <- tm_map(mt.c, stripWhitespace)

inspect(mt.c.p)

dtm <- TermDocumentMatrix(mt.c.p)
m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word= names(v), freq=v)

head(d, 50)

write_as_csv(d, "~/Documents/Teaching/PalmDrive/1.25.2021 PBL/PBL_ppt/#COVID-19_fre_words.csv",
             prepend=TRUE, na = "", fileEncoding = "UTF-8")

install.packages("wordcloud2")
library(wordcloud2)

set.seed(1234)

wordcloud2(d, size=2, color='random-light')

#Exploration 3 : active users
mt2.v <- VectorSource(Tweets_data$user_id)
mt2.c <- SimpleCorpus(mt2.v)

inspect(mt2.c)

mt2.c.p <- tm_map(mt2.c, content_transformer(tolower))
mt2.c.p <- tm_map(mt2.c, removeNumbers)
mt2.c.p <- tm_map(mt2.c, removeWords, stopwords('english'))
mt2.c.p <- tm_map(mt2.c, removePunctuation)
mt2.c.p <- tm_map(mt2.c, stripWhitespace)

inspect(mt2.c.p)

dtm2 <- TermDocumentMatrix(mt2.c.p)
m2 <- as.matrix(dtm2)
v2 <- sort(rowSums(m2), decreasing = TRUE)
d2 <- data.frame(word= names(v2), freq=v2)

active <- head(d2, 10)
active
write_as_csv(active, "~/Documents/Teaching/PalmDrive/1.25.2021 PBL/PBL_ppt/#COVID-19_active_users.csv",
             prepend=TRUE, na = "", fileEncoding = "UTF-8")
#Who are they? Why they tweet so frequently? What are their tweets talking about?

#Data cleaning?

#Definition??????????????????????????????>Measurement
#Research question

######################### Data analysis #########################
# Text Pre-processing 
#see R code ???Russian Twitter Accounts and the Partisan Polarization of Vaccine Discourse, 2015-2017???

## Topic modeling-Latent Dirichelet Allocation(lDA)
#a computational text nalysis method used to find ???latent??? thematic structure of a given textual dataset.

#LDA topic modeling does not require prior knowledge of topics???i.e., the computer ???reads??? the texts for you.
install.packages("lda")
install.packages("LDAvis")

library(lda)
library(LDAvis)
library(stringr)
#https://www.bilibili.com/video/BV1N54y1t7e2/?spm_id_from=333.788.videocard.0
#see R code ???Russian Twitter Accounts and the Partisan Polarization of Vaccine Discourse, 2015-2017???

##Sentiment analysis:
#Unsupervised machine learning: https://www.bilibili.com/video/BV1Jt4y1k7qn/?spm_id_from=333.788.recommend_more_video.-1
#Dictionary: https://www.tidytextmining.com/sentiment.html 


#Network analysis
## if you haven't installed the relevant packages, do so with the following code:
#install.packages("network")
#install.packages("statnet")
#install.packages("sna")
#install.packages("numDeriv")

#loading the packages
#library(network)
#library(statnet)
#library(numDeriv)
#library(sna)