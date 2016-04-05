library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(glmnet)
library(maptools)
library(ggmap)

#Read data 
test <- read.csv("test.csv")
train <- read.csv("train.csv")
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
ocurrences = ddply(train_set,.(Category), summarize,ocurrences=length(Category) / dim(train_set)[1])
ocurrences = ocurrences[order(ocurrences$ocurrences, decreasing = TRUE),]
head(ocurrences)

ggplot(ocurrences[ocurrences$ocurrences>=0.025,], aes(x=Category,ocurrences,fill=Category)) +geom_bar(stat = 'identity')
sum(ocurrences$ocurrences[ocurrences$ocurrences >=0.025])

sig_cats = (ocurrences$Category[ocurrences$ocurrences >= 0.025])
sig_levels = levels(as.factor(as.character(sig_cats)))

sig_train = train_set[train_set$Category %in% sig_levels & train_set$Y < 50,]
#crimes with colour per category
ggplot(sig_train, aes(x=X,y=Y)) + geom_point(aes(colour=factor(Category)))
#crimes with colour per category in a panel per year
ggplot(sig_train, aes(x=X,y=Y)) + geom_point(aes(colour=factor(Category))) + facet_wrap(~Year)
#crimes with colour per category in a panel per hour
ggplot(sig_train, aes(x=X,y=Y)) + geom_point(aes(colour=factor(Category))) + facet_wrap(~Hour)

ocurrences_year = ddply(sig_train,.(Category,Year), summarize,ocurrences=length(Category))
ocurrences_year$Year = as.factor(ocurrences_year$Year)

ggplot(ocurrences_year, aes(x=Category,ocurrences, fill=Category)) +geom_bar(stat = 'identity') + facet_wrap(~Year)
ggplot(ocurrences_year, aes(x=Year,ocurrences,fill=Category)) + geom_bar(stat = 'identity')

ocurrences_yearp = ddply(ocurrences_year,.(Year),summarize,ocurrencesp = sum(ocurrences))
ocurrences_yearp = merge(ocurrences_year,ocurrences_yearp,by="Year")
head(ocurrences_yearp)

ocurrences_yearp$ocurrencesp = ocurrences_yearp$ocurrences / ocurrences_yearp$ocurrencesp
head(ocurrences_yearp)
ocurrences_yearp = ocurrences_yearp[order(ocurrences_yearp$Year,ocurrences_yearp$ocurrencesp, decreasing = TRUE),]

ggplot(ocurrences_yearp, aes(x=Year,ocurrencesp,fill=Category)) + geom_bar(stat='identity')

ocurrences_yearp = ocurrences_yearp[order(ocurrences_yearp$Year,ocurrences_yearp$Category),]
ggplot(ocurrences_yearp, aes(x=Year,ocurrencesp,fill=Category)) + geom_bar(stat = 'identity')

ocurrences_district = ddply(sig_train,.(Category,PdDistrict,Hour),summarize,ocurrences =length(Category))
ocurrences_total = ddply(sig_train,.(PdDistrict),summarize, t_oc = length(PdDistrict))
ocurrences_district = merge(ocurrences_district,ocurrences_total,by="PdDistrict")
ocurrences_district$oc_perc = ocurrences_district$ocurrences / ocurrences_district$t_oc
ocurrences_district = ocurrences_district[order(ocurrences_district$PdDistrict,ocurrences_district$Category,ocurrences_district$oc_perc,decreasing = TRUE),]

ggplot(ocurrences_district, aes(x=PdDistrict,oc_perc,fill=Category)) + geom_bar(stat = 'identity') + facet_wrap(~PdDistrict)


max_cat=ddply(ocurrences_district,.(PdDistrict,Hour),summarize,max_oc=max(ocurrences),cat_max=Category[which.max(ocurrences)])
ggplot(max_cat,aes(x=PdDistrict,y=Hour))+geom_point(aes(size=max_oc,colour=cat_max))


san_fran = get_map(location='San Francisco', zoom = 12, maptype = 'toner')
ggplot(san_fran)
map = readShapeLines('bayarea_general/bayarea_general.shp')
map@data$id = rownames(map@data)
map1 = fortify(map, region ="id")
ggplot(map1) + geom_polygon(aes(x=long,y=lat,group=group))

map2 = map1[order(map1$order),]

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
