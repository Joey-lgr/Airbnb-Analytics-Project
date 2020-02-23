df <- read.csv("melbourne.csv") 
library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally) 
library(mice)
# filter the only neighborhoodL Southbank, from Melbourne 
df1 <- filter(df, neighborhood=="Southbank") 
#select variables from the data set and make up a new data frame 
mlr1 <- select(df1, price, accommodates, bathrooms,
               cleaning_fee, host_is_superhost, review_scores_value,
               reviews_per_month, security_deposit, 
               instant_bookable, availability_30, 
               calculated_host_listings_count) 
#clean data
mlr1$host_is_superhost[mlr1$host_is_superhost == ""] <- NA 
mlr2 <- na.omit(mlr1) #omit the na values in the data set 
mlr2 <- droplevels(mlr2) #check no ghost level 
md.pattern(mlr2) #check the missing values in the data frame 
lm(price~., mlr2) #overview of the categorical variables 
#dummy variable for categorical values
mlr2$host_is_superhost <- ifelse(mlr2$host_is_superhost=="t", 1, 0) 
mlr2$instant_bookable <- ifelse(mlr2$instant_bookable=="t", 1, 0) 
#set seed and slice data 
set.seed(180)
dim(mlr2)
dfsample <- sample_n(mlr2, 774)
774*0.6  #assign 60% to train data 
train <- slice(dfsample, 1:465)
valid <- slice(dfsample, 466:774)
#check multicollinearity pairs through train data set 
model.var <- select(train, accommodates, bathrooms,
                 cleaning_fee, host_is_superhost, review_scores_value,
                 reviews_per_month, security_deposit, 
                 instant_bookable, availability_30, 
                 calculated_host_listings_count) 
ggpairs(model.var) 
#cor are all lower than 0.6 
model1 <- lm(price~., train)
summary(model1)
backmodel <- step(model1, direction="backward")
summary(backmodel) #adjusted R-squared increases 
model2 <- lm(price~accommodates+bathrooms+guests_included+
                reviews_per_month+security_deposit+availability_30+calculated_host_listings_count, train)
summary(model2)
library(visualize) #t distribution 
visualize.t(stat=c(-1.987, 1.987), df=457, section = "bounded") #0.952 very reliable 
#fake date frame
range(cormlr$availability_30)
range(cormlr$calculated_host_listings_count)
myhouse <- data.frame(accommodates=2, bathrooms=2, guests_included=2, 
                      reviews_per_month=5, security_deposit=0, availability_30=20, 
                      calculated_host_listings_count=5)

predict(model2, myhouse)
-14.177 + 24.11*2 + 57.59*2 + 9.36*2 + -13.03*5 + 0.029*0 + 1.95*20 + -0.72*5

#accuracy test 
library(forecast)
pred1 <- predict(model2, train)
accuracy(pred1, train$price)
pred2 <- predict(model2, valid)
accuracy(pred2, valid$price)

