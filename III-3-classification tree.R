library(tidyverse)
library(psych)
library(pastecs)
library(dplyr) 
library(mice)
library(rpart)
library(rpart.plot) 
library(caret)
# get data
df <- read.csv("melbourne.csv")
mydata <- filter(df,df$neighborhood=="Southbank")
mydata1 <- select(mydata, room_type, accommodates, bathrooms, bedrooms, beds, price, cleaning_fee)
# drop NA
md.pattern(mydata1) 
mydata1 <- na.omit(mydata1)
anyNA(mydata1)
# bin cleaning_fee
summary(mydata1$cleaning_fee)
mydata1$cleaning_fee <- cut(mydata1$cleaning_fee,breaks=c(-1,0,35,73,450),labels=c("No Fee","Low","Medium","High"))
# perpare training set and validation set
set.seed(180)
mydata2 <- sample_n(mydata1, 837)
mydata2_train <- slice(mydata2, 1:503)
mydata2_valid <- slice(mydata2, 504:837)
# buid a tree
model1<-rpart(cleaning_fee~.,data=mydata2_train,method="class",minsplit=2,minbucket=1)
rpart.plot(model1,type = 1,extra = 1,under = TRUE, fallen.leaves = FALSE,tweak=1.2,box.palette = 0)
# get a optimal CP value
model2<-rpart(cleaning_fee~.,data=mydata2_train,method="class",cp=0.00001,xval=503)
printcp(model2) # cp = 
# build a new tree
model3<-prune(model2,cp=0.0039526)
length(model3$frame$var[model3$frame$var=="<leaf>"])
rpart.plot(model3,type = 1,extra = 1,under = TRUE, fallen.leaves = FALSE,tweak=1.2,box.palette = 0)
# get the tree's accuracy against the training set
train.pred<-predict(model3,mydata2_train,type="class")
confusionMatrix(train.pred,mydata2_train$cleaning_fee)
# get the tree's accuracy against the validation set
valid.pred<-predict(model3,mydata2_valid,type="class")
confusionMatrix(valid.pred,mydata2_valid$cleaning_fee)



