library(dplyr)
library(FNN)
library(caret)
library(ggplot2) 
library(tidyr)
library(e1071)
library(mice)
# get data
survey <- read.csv("melbourne.csv")
neihbor <- filter(survey,survey$neighborhood == "Southbank")
predictor <- select(neihbor, cancellation_policy, host_response_rate, price, security_deposit, review_scores_communication)
# process data
predictor$host_response_rate<-as.numeric(sub("%","",predictor$host_response_rate))/100
predictor$host_response_rate[predictor$host_response_rate == "N/A"]<-NA
predictor$host_response_rate[predictor$host_response_rate == ""]<-NA
predictor$review_scores_communication[predictor$review_scores_communication == "N/A"]<-NA
predictor$review_scores_communication[predictor$review_scores_communication == ""]<-NA
predictor$security_deposit[predictor$security_deposit == "N/A"]<-NA
predictor$security_deposit[predictor$security_deposit == ""]<-NA
predictor1 <- na.omit(predictor)
anyNA(predictor1)
#  divide into training set and validation set
set.seed(180)
predictor2 <- sample_n(predictor1, 689)
train.df <- slice(predictor2, 1:414)
valid.df <- slice(predictor2, 415:689)
# establish a new rental
host_response_rate <- runif(1, min(predictor1$host_response_rate),max(predictor1$host_response_rate))
review_scores_communication <-runif(1, min(predictor1$review_scores_communication),max(predictor1$review_scores_communication))
security_deposit <-runif(1, min(predictor1$security_deposit),max(predictor1$security_deposit))
price <-runif(1, min(predictor1$price),max(predictor1$price))
new.rental <- data.frame(host_response_rate, review_scores_communication, security_deposit, price)
# normalization data
train.norm.df <- train.df
valid.norm.df <- valid.df
norm.values <- preProcess(train.df[, 2:5], method=c("center", "scale"))
train.norm.df[, 2:5] <- predict(norm.values, train.df[, 2:5])
valid.norm.df[, 2:5] <- predict(norm.values, valid.df[, 2:5])
new.rental.norm.df <- predict(norm.values, new.rental)
# build knn model
nn <- knn(train = train.norm.df[, 2:5], test = new.rental.norm.df, cl = train.norm.df[, 1], k = 7)
row.names(train.df)[attr(nn, "nn.index")]

new.rental.neighbor <- train.df[c(109,16,394,211,273,174,133),]
new.rental.neighbor$cancellation_policy #my policy is flexible
# 
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))
for(i in 1:14) {
  knn.pred <- knn(train.norm.df[, 2:5], valid.norm.df[, 2:5], 
                  cl = train.norm.df[, 1], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.df[, 1])$overall[1] 
}
accuracy.df # the best is that k=12
# vislization about K and accuracy
ggplot(accuracy.df, aes(x=accuracy.df$k, y=accuracy.df$accuracy))+geom_point()
#
knn12 <-knn(train=train.norm.df[,2:5],test=new.rental.norm.df,cl=train.norm.df[,1],k=12)
knn12
row.names(train.df)[attr(knn12,"nn.index")]

new.rental.neighbor_valid <- train.df[c(109,16,394,211,174,133,161,370,223,282,106),]
new.rental.neighbor_valid$cancellation_policy #my policy is flexible



