library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(glmnet)
library(maptools)

#Read data 
test <- read.csv("~/Repositories/SF_Crimes/test.csv")
train <- read.csv("~/Repositories/SF_Crimes/train.csv")
head(train$Dates)

# clean and transform data
train$Dates = as.character(train$Dates)

get_date = function(string){
  date = strsplit(string," ")[[1]][1]
}

get_hour = function(string){
  date = strsplit(string," ")[[1]][2]
}

train$TimeStamp = train$Dates
train$Date = sapply(train$TimeStamp,get_date)
train$Date = as.Date(train$Date)

train$Time = sapply(train$TimeStamp,get_hour)
train$Hour = sapply(train$Time,substr,1,2)
train$Hour = as.numeric(train$Hour)
train$Year = year(train$Date)
head(train$PdDistrict)

# split data into train and validation sets
# phase 1: visualization of 10,000 cases
set.seed(123)


smp_size = dim(train)[1]*0.8
smp_size = 10000
sample_id=sample(seq_len(nrow(train)), size = smp_size)
train_set = train[sample_id,]

# Summaries and Visualizations....
ocurrences = ddply(train_set,.(Category), summarize,ocurrences=length(Category))
ocurrences_year = ddply(train_set,.(Category,Year), summarize,ocurrences=length(Category))

ggplot(ocurrences, aes(x=Category,ocurrences)) +geom_bar(stat = 'identity')


y = train_set$Category
val_set = train[-sample_id,]
train_x = data.matrix(train_set[,c("Hour","DayOfWeek","PdDistrict","Year")])

model = cv.glmnet(train_x, train_set$Category, family="multinomial", alpha=1)

vect = predict(model,newx = train_x[1:30,],type = "response",s=0.01)
preds =matrix(vect,ncol=39,nrow=30,byrow=FALSE)

get_pred_class = function(probs){
  class = which.max(probs)
  return(class)
}
