library(tidyverse)
library(dplyr)
library(mice)
library(Hmisc)
library(BBmisc)
library(cluster)
library(factoextra)
library(dummy)


# get data
df <- read.csv("melbourne.csv")
mydata <- filter(df,df$neighborhood=="Southbank")
# select variables subjectively 
mydata1 <- select(mydata, 
                  accommodates, 
                  bathrooms, bedrooms, beds, 
                  price, 
                  review_scores_rating,
                  room_type)
#which variables has NA
md.pattern(mydata1) 
# use mean to replace NA
mydata1$beds[is.na(mydata1$beds)] <- mean(mydata1$beds, na.rm = T)
mydata1$review_scores_rating[is.na(mydata1$review_scores_rating)] <- mean(mydata1$review_scores_rating, na.rm = T)
anyNA(mydata1) 
# a dataframe includes numerica variables
accommodates <- mydata1$accommodates
comfort <- mydata1$bathrooms*.35 + mydata1$bedrooms*.3 + mydata1$beds*.35
price <- mydata1$price
evaluation <- mydata1$review_scores_rating
mydata2 <- data.frame(accommodates,comfort,price,evaluation)
# add dummy variable
dumy <- dummy(x=mydata1,p = "all")
mydata_3 <- cbind(mydata2,dumy)
# turn factor to numeric
mydata_3 <- transform(mydata_3, room_type_Entire.home.apt=as.numeric(as.character(room_type_Entire.home.apt)))
mydata_3 <- transform(mydata_3, room_type_Private.room=as.numeric(as.character(room_type_Private.room)))
mydata_3 <- transform(mydata_3, room_type_Shared.room=as.numeric(as.character(room_type_Shared.room)))
str(mydata_3)
# normalization
mydata3 <- sapply(mydata_3, scale) 
# add id
row.names(mydata3) <- mydata[,1]
# gap stastistics to determine k value
gap_clust <-clusGap(mydata3, kmeans, nstart = 25, K.max = 8, B = 100, verbose = interactive())
gap_clust 
fviz_gap_stat(gap_clust)
# k-mean
km <- kmeans(mydata3, 5, nstart=25)
km$centers
# Visualize clusters 
fviz_cluster(km, mydata3,geom = c("point"))

