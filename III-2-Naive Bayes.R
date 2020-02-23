library(tidyverse)
library(forecast)
library(e1071)
library(dplyr)
library(caret)
library(ggplot2) 
#Naive Bayes
df<-read.csv("melbourne.csv")
df2<-filter(df,neighborhood=="Southbank")
df3<-df2[,c(79,23,48,62,80)]
#NAs
str(df3)
df3$host_response_rate<-as.numeric(sub("%","",df3$host_response_rate))/100
df3$instant_bookable[df3$instant_bookable == "N/A"]<-NA
df3$instant_bookable[df3$instant_bookable == ""]<-NA
df3$cancellation_policy[df3$cancellation_policy == "N/A"]<-NA
df3$cancellation_policy[df3$cancellation_policy == ""]<-NA
df3<-na.omit(df3)
anyNA(df3)
md.pattern(df3)
#partition: training set-60% | validation set-40%
set.seed(300)
dim(df3)
seed<-sample_n(df3, 963)
963*0.6
train<-slice(seed,1:578)
valid<-slice(seed,579:963)
#build a model using the naive Bayes algorithm
naive<- naiveBayes(instant_bookable ~ cancellation_policy+beds+availability_30
                   +host_response_rate,data =train)
naive

#test
pre.train <- predict(naive, newdata =train)
confusionMatrix(pre.train, train$instant_bookable)
pre.valid <- predict(naive, newdata =valid)
confusionMatrix(pre.valid, valid$instant_bookable)

#create a fictional apartment
ficational<-data.frame(beds=3,
                       cancellation_policy=as.factor("flexible"),
                       availability_30=24,
                       host_response_rate=1)
predict(naive,ficational)
