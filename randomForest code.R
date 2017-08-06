#Random forest
library(MASS)
library(randomForest)
library(dplyr)
library(readr)
set.seed(222)
trainR<-read.csv("C:/Users/Tanuja/Desktop/eBayiPadTrain.csv")
testR<-read.csv("C:/Users/Tanuja/Desktop/eBayiPadTest.csv")
str(trainR)
summary(trainR)
apply(trainR,2,function(x) length(unique(x)))
trainR$description<-as.character(trainR$description)
testR$description<-as.character(testR$description)
trainR$desc<-ifelse(nchar(trainR$description)>0,1,0)
testR$desc<-ifelse(nchar(testR$description)>0,1,0)
trainR<-trainR[-which(trainR$productline=="iPad 5"),]
trainR$condition<-as.character(trainR$condition)
testR$condition<-as.character(testR$condition)
trainR$condition[trainR$condition=="Manufacturer refurbished"|trainR$condition=="Seller refurbished"]<-"Refurbished"
testR$condition[testR$condition=="Manufacturer refurbished"|testR$condition=="Seller refurbished"]<-"Refurbished"
colss<-c("biddable","cellular","storage","sold","desc","condition","carrier","productline","color")
for(i in colss){
trainR[,i]=as.factor(trainR[,i])
#testR[,i]=as.factor(testR[,i])
}
colsss<-c("biddable","cellular","storage","desc","condition","carrier","productline","color")
for(i in colsss){
  #trainR[,i]=as.factor(trainR[,i])
  testR[,i]=as.factor(testR[,i])
}
str(trainR)
str(testR)

library(caTools)
trainR<-droplevels(trainR)
testR<-droplevels(testR)
trainR$sold<-as.factor(trainR$sold)
table(trainR$sold)
reg<-floor(12/3)
reg
cat<-floor(sqrt(11))
cat

trainR<-subset(trainR,!is.na(biddable))
str(trainR)
trainR<-subset(trainR,!is.na(startprice))
str(trainR)
trainR<-subset(trainR,!is.na(condition))
str(trainR)
trainR<-subset(trainR,!is.na(storage))
str(trainR)
trainR<-subset(trainR,!is.na(productline))
str(trainR)
trainR<-subset(trainR,!is.na(carrier))
str(trainR)
trainR<-subset(trainR,!is.na(color))
str(trainR)
trainR<-subset(trainR,!is.na(cellular))
str(trainR)
trainR<-na.omit(trainR)

testR<-subset(testR,!is.na(biddable))
str(trainR)
testR<-subset(testR,!is.na(startprice))
str(trainR)
testR<-subset(testR,!is.na(condition))
str(trainR)
testR<-subset(testR,!is.na(storage))
str(trainR)
testR<-subset(testR,!is.na(productline))
str(trainR)
testR<-subset(testR,!is.na(carrier))
str(trainR)
testR<-subset(testR,!is.na(color))
str(trainR)
testR<-subset(testR,!is.na(cellular))
str(trainR)
testR<-na.omit(testR)

trainR$desc<-as.factor(trainR$desc)

set.seed(1000)
library(caTools)
split <- sample.split(trainR$sold, SplitRatio = 0.7)
train  <- filter(trainR, split == T)
test <- filter(trainR, split == F)


set.seed(1000)
model_rf <- randomForest(sold ~ biddable+startprice+condition+cellular+carrier+color+storage+productline+UniqueID+desc, data = train, importance = T)
predict_rf  <- predict(model_rf, newdata = test, type = "prob")[,2]
ROCRpred = prediction(predict_rf, test$sold)
AUC=as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

levels(testR$biddable)<-levels(trainR$biddable)
levels(testR$cellular)<-levels(trainR$cellular)
levels(testR$storage)<-levels(trainR$storage)
levels(testR$desc)<-levels(trainR$desc)
levels(testR$condition)<-levels(trainR$condition)
levels(testR$carrier)<-levels(trainR$carrier)
levels(testR$color)<-levels(trainR$color)
levels(testR$productline)<-levels(trainR$productline)

predict_rf_test = predict(model_rf, newdata=testR,type = "prob")

outputR<-data.frame(UniqueID=testR$UniqueID,prob=predict_rf_test)
setwd("/Users/Tanuja/Desktop")
write.csv(outputR,"Random_Forest.csv",row.names = FALSE)
AUC
#0.8471641

