# Date: 03/19/2017  
# Proj: Quora Kaggle Competetion 
# Name: Nachiket Garge

rm(list = ls())
# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(stringr)
library(tm)
library(syuzhet)
library(SnowballC)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

# Any results you write to the current directory are saved as output.

train <-read_csv("C:/Users/Nachiket Garge/Downloads/Quora_kaggle/train.csv",n_max = 1000)
test <-read_csv("C:/Users/Nachiket Garge/Downloads/Quora_kaggle/test.csv", n_max = 1000)
dim(train)

# For each question preparing a score with same important words
# For eg. If we consider question8
# train$question1[8]
# "How can I be a good geologist?"
# train$question2[8]
# "What should I do to be a great geologist?"
# After data filtering, stemming, removing puctuations, removing stop words and converting all words to lower case
# we have following results,
# > a11
#[1] "can"       "geologist" "good"     
#> b11
#[1] "geologist" "great"
# Now calculating score with common words
# score = 2/5 (as there are 2 common words within total of 5 words)
# We can predict the target with help of this score

# This method may lead to mis interpretation in case of opposite sentences 
# Thus,
# Through sentiment analysis we can provide positive and negative emotions as predictors to improve accuracy

# With these regressors I achieved the accuracy of 69% 

# Please find the code below:

df = data.frame()
df.new = data.frame()
for (i in 1:nrow(train))
{
# cbind(train,adist = diag(adist(train$question1,train$question2)))
q1 <- Corpus(VectorSource(train$question1[i]))

# Performing the Text cleaning
#Removing punctuations
q1 <- tm_map(q1, removePunctuation)   
#inspect(q1)
# To remove punctuations on the text data

#Removing Numbers removeNumbers
q1 <- tm_map(q1, removeNumbers)   
#inspect(q1)

#To avoid duplicacy converting all to lower case tolower
q1 <- tm_map(q1, tolower)   
#inspect(q1)

# removing common word endings stemDocument
q1 <- tm_map(q1, stemDocument)   
#inspect(q1)

# to remove white space stripWhitespace
q1 <- tm_map(q1, stripWhitespace)   
#inspect(q1)
#Removing Stop Words as they don't add any value
#Smart is inbuilt, which gives a set of pre-defined stop words.  removeWords, stopwords("english")
q1 <- tm_map(q1, removeWords, stopwords("english"))   
#inspect(q1)
# to convert documents into text documents...this tells R to treat preprocessed document as text documents PlainTextDocument
q1 <- tm_map(q1, PlainTextDocument)   
#inspect(q1)

doc = TermDocumentMatrix(q1) 
a11 = doc$dimnames$Terms


q2 <- Corpus(VectorSource(train$question2[i]))

# Performing the Text cleaning
#Removing punctuations
q2 <- tm_map(q2, removePunctuation)   
# To remove punctuations on the text data

#Removing Numbers removeNumbers
q2 <- tm_map(q2, removeNumbers)   

#To avoid duplicacy converting all to lower case tolower
q2 <- tm_map(q2, tolower)   

# removing common word endings stemDocument
q2 <- tm_map(q2, stemDocument)   

# to remove white space stripWhitespace
q2 <- tm_map(q2, stripWhitespace)   

#Removing Stop Words as they don't add any value
#Smart is inbuilt, which gives a set of pre-defined stop words.  removeWords, stopwords("english")
q2 <- tm_map(q2, removeWords, stopwords("english"))   

# to convert documents into text documents...this tells R to treat preprocessed document as text documents PlainTextDocument
q2 <- tm_map(q2, PlainTextDocument)   


doc = TermDocumentMatrix(q2) 
b11 = doc$dimnames$Terms

#a11
#b11

c11 = a11 %in% b11
same_items = sum(c11)
distinct_items = length(a11) + length(b11)
#distinct_items

match_count = (2*same_items)/(distinct_items)
#match_count


# train$is_duplicate[8]
# train$question1[8]
# train$question2[8]

sentiment1 <- get_nrc_sentiment(train$question1[i])
sentiment2 <- get_nrc_sentiment(train$question2[i])

sentiment1
sentiment2

p1 = sum(sentiment1$positive)
p2 = sum(sentiment2$positive)

n1 = sum(sentiment1$negative)
n2 = sum(sentiment2$negative)

# n1
# n2
# 
# p1
# p2
df.new = cbind(match_count,p1,p2,n1,n2)
df = rbind(df,df.new)
}

tr = cbind(train,df)
###################################################################################################
tr = tr[,6:11]
tr[,c(1,3:6)] = lapply(tr[,c(1,3:6)],as.factor)
tr = na.omit(tr)

library(randomForest)
model <- randomForest(is_duplicate ~ ., data = tr)
model

pred <- predict(model, newdata = tr,  type="prob")

#bound the results, otherwise you might get infinity results
pred = apply(pred, c(1,2), function(x) min(max(x, 1E-15), 1-1E-15)) 

logLoss = function(pred, actual){
  -1*mean(log(pred[model.matrix(~ actual + 0) - pred > 0]))
}


### Logloss value = 
logLoss(pred, tr$is_duplicate)

write.csv(pred[,1],"words_benchmark.csv")

