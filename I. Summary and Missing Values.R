df <- read.csv("melbourne.csv") 
library(tidyverse)
library(dplyr) 
library(tidytext)
library(wordcloud)
library(ggplot2)
library(lubridate)
################################################################################################
# filter the only neighborhoodL Southbank, from Melbourne 
df1 <- filter(df, neighborhood=="Southbank") 
names(df1)
#select variables from the data set and make up a new data frame 
df2 <- select(df1, accommodates,bedrooms,beds, bathrooms, price, 
              review_scores_rating, cleaning_fee,
              review_scores_value,review_scores_location, review_scores_accuracy, 
              review_scores_communication, review_scores_cleanliness, host_response_rate) 

#I. Missing Values 
df2$host_response_rate[df2$host_response_rate == "N/A"]<-NA  #change the N/A value to NA
df2$host_response_rate[df2$host_response_rate == ""]<-NA     #change blank cells value to NA
#tell us the missing values, this gives the number of null values: 
sum(is.na(df2))  #relationship of two or more variables 
df3 <- na.omit(df2)   #drop NA values 
df33 <- droplevels(df3)   #make sure there is no ghost row in the dataset (no ghost)
df3 <- as.numeric(df3$host_response_rate)
host_response_rate <- as.numeric(sub("%","",df3$host_response_rate))/100
df3$host_response_rate  <- host_response_rate 
sum(is.na(df3))  #none missing values anymore
anyNA(df3)   #double-check...

#II. Summary Statistics 

#what are the keywords in property descirptions? (txt mining)
library(tm)
library(e1071)
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus,stripWhitespace)
  corpus <- tm_map(corpus,removePunctuation)
  corpus <- tm_map(corpus,removeNumbers)
  corpus <- tm_map(corpus,content_transformer(tolower))
  corpus <- tm_map(corpus,removeWords,stopwords("en"))
  corpus <- tm_map(corpus,stemDocument)
  return(corpus)
}
dff <- select(df1,neighborhood_overview)
dff$neighborhood_overview[dff$neighborhood_overview == ""] <- NA 
dff <- na.omit(dff)   #drop NA values 
sum(is.na(dff))
descrip <- VCorpus(VectorSource(dff))
descrip <- clean_corpus(descrip)
tdm1 <- TermDocumentMatrix(descrip)
inspect(tdm1)
mtx1 <- as.matrix(tdm1)
term_frequency1 <- rowSums(mtx1)
term_frequency1 <- sort(term_frequency1,decreasing = TRUE)
word_freqs1 <- data.frame(term = names(term_frequency1), num = term_frequency1)
wordcloud(word_freqs1$term, word_freqs1$num, min.freq = 200, colors = "blue")

#1.How does airbnb in Southbank perform? 
summary_score <- data.frame(mean <- sapply(df3, mean),
                       sd <- sapply(df3, sd),
                       median <- sapply(df3, median),
                       max <- sapply(df3, max),
                       min <- sapply(df3, min)) 


#2. What are the property types? 
type <- select(df1, host_is_superhost, property_type, room_type, bed_type)
type$host_is_superhost[type$host_is_superhost == ""] <- NA 
type<- na.omit(type) #most complete data set rom these types 
type <- droplevels(type)
pt <- table(type$property_type)
pt2 <- sort(prop.table(pt), decreasing = TRUE)
pt3 <- table(head(pt2, 10))
roomtype <- data.frame(table(type$room_type))
names(roomtype)[names(roomtype) == "Var1"] <- "PropertyType"
names(roomtype)[names(roomtype) == "Freq"] <- "Numbers"
roomtype

rt <- table(type$room_type)
rt2 <- sort(prop.table(rt), decreasing = TRUE)
rt3 <- head(rt2, 3)
rt3
bt <- table(type$bed_type)
bt2 <- sort(prop.table(bt), decreasing = TRUE)
bt3 <- head(bt2, 3)
bt3

#############################################################################################
#III. Visualization : property - superhost - price - review - listings 
#1 possible reasons for whether the host is a superhost - line graph
a <- table(type$host_is_superhost)
prop.table(a)
ggplot(type, aes(x=room_type)) + geom_bar() + facet_grid(~host_is_superhost) + 
  ggtitle("Does room type make the host become a superhost?") + theme(plot.title = element_text(hjust = 0.5))

ggplot(type, aes(x=bed_type)) + geom_bar() + facet_grid(~host_is_superhost) + 
  ggtitle("Does bed type make the host become a superhost?") + theme(plot.title = element_text(hjust = 0.5))
#no relation with any of the types above, 
#but it shows the proportion of what is the most option that hosts offer 
#2. property type - pie chart 
library(scales)
b <- ggplot(roomtype, aes(x="", y=Numbers, fill=PropertyType)) + geom_bar(width = 1, stat = "identity")
b
pie <- b + coord_polar("y", start=0) + 
  geom_text(aes(label=paste0(round(roomtype$Numbers/sum(roomtype$Numbers)*100),"%")), 
            position = position_stack(vjust = 0.5)) + 
  ggtitle("Proportions of Properties") + theme(plot.title = element_text(hjust = 0.5))
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))
pie
#3. price fluctation graph - histgram - since our data is not huge, 
#I will keep as it looks, instead of normalization....
c1 <- ggplot(df3, aes(x=price)) 
c1 + geom_histogram(bins=30, color = "black", fill="light blue") +
  geom_vline(aes(xintercept = mean(price)),linetype="dashed",size=0.6) + 
  ggtitle("Price Fluctation in South Bank Neighborhood") + theme(plot.title = element_text(hjust = 0.5))

c2 <- ggplot(df3, aes(x=price)) + xlim(0,400) #ignore outliners are larger than 400
c2 + geom_histogram(bins=30, color = "black", fill="light blue") +
  geom_vline(aes(xintercept = mean(price)),linetype="dashed",size=0.6) + 
  ggtitle("Price Fluctation in South Bank Neighborhood") + theme(plot.title = element_text(hjust = 0.5))
mean(df3$price) #182 
#4 review score graph - histgram
d1 <- ggplot(df3, aes(x=review_scores_rating))
d1 + geom_histogram(bins=30, color = "black", fill="light pink")+
  geom_vline(aes(xintercept = mean(review_scores_rating)),linetype="dashed",size=0.6)+
  ggtitle("Score Review in South Bank Neighborhood") + theme(plot.title = element_text(hjust = 0.5))

d2 <- ggplot(df3, aes(x=review_scores_rating)) + xlim(78,100) #enlarge the graph and see clear distribution 
d2 + geom_histogram(bins=30, color = "black", fill="light pink")+
  geom_vline(aes(xintercept = mean(review_scores_rating)),linetype="dashed",size=0.6)+
  ggtitle("Score Review in South Bank Neighborhood") + theme(plot.title = element_text(hjust = 0.5))
mean(df3$review_scores_rating) #94.4

#5 listings through years
df4 <- select(df1, host_since, calculated_host_listings_count) 
sum(is.na(df4))
df4$host_since[df4$host_since == ""] <- NA 
df4 <- na.omit(df4)
df4$Year <- as.Date(df4$host_since, "%m/%d/%Y")
df4$Year <- as.numeric(format(df4$Year, "%Y"))
str(df4)
df5 <- group_by(df4, Year) %>%
  summarize(listings=sum(calculated_host_listings_count))

ggplot(df5, aes(x=Year, y=listings)) + geom_line(size=1.5, color="chocolate4") + geom_point(size=2) + 
  ggtitle("Host Listings Count Through Years ") + theme(plot.title = element_text(hjust = 0.5))






