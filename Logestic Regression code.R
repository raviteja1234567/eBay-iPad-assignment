library(tm)
library(StatMatch)
library(caret)
library(ROCR)
library(Metrics)
library(ggplot2)
set.seed(144)
setwd("/Users/Tanuja/Desktop")
train<-read.csv("C:/Users/Tanuja/Downloads/eBayiPadTrain.csv")
test<-read.csv("C:/Users/Tanuja/Downloads/eBayiPadTest.csv")
#train$UniqueID=NULL
train$condition.code=NULL
train$carrier.code=NULL
train$color.code=NULL
train$product.code=NULL
train$storage.code=NULL
train$cellular.code=NULL
#test$UniqueID=NULL
test$condition.code=NULL
test$carrier.code=NULL
test$color.code=NULL
test$product.code=NULL
test$storage.code=NULL
test$cellular.code=NULL
test$X=NULL
test$X.1=NULL
test$X.2=NULL
test$X.3=NULL
test$ipadname=NULL
test$code=NULL
test$cellularr=NULL
test$code.1=NULL
train$description<-as.character(train$description)
test$description<-as.character(test$description)
train$desc<-ifelse(nchar(train$description)>0,1,0)
test$desc<-ifelse(nchar(test$description)>0,1,0)
train<-train[-which(train$productline=="iPad 5"),]
t<-subset(test,test$cellular=="Other")
train$carrier<-as.factor(train$carrier)
test$carrier<-as.factor(test$carrier)
train<-droplevels(train)
test<-droplevels(test)
train$condition<-as.character(train$condition)
test$condition<-as.character(test$condition)
train$condition[train$condition=="Manufacturer refurbished"|train$condition=="Seller refurbished"]<-"Refurbished"
test$condition[test$condition=="Manufacturer refurbished"|test$condition=="Seller refurbished"]<-"Refurbished"
hist(train$startprice,col="tomato",xlab = "Start Price",main = "Frequency Distribution of startprice")
train$startprice<-log(train$startprice)
hist(train$startprice,col = "tomato",xlab = "start price",main = "Frequency Distribution of startprice",xlim = c(2,8),breaks = 25)
test$startprice<-log(test$startprice)
CorpusDesc<-Corpus(VectorSource(c(train$description,test$description)))
CorpusDesc<-tm_map(CorpusDesc,tolower)
CorpusDesc<-tm_map(CorpusDesc,PlainTextDocument)
CorpusDesc<-tm_map(CorpusDesc,removePunctuation)
CorpusDesc<-tm_map(CorpusDesc,removeWords,stopwords("english"))
CorpusDesc<-tm_map(CorpusDesc,stripWhitespace)
CorpusDesc<-tm_map(CorpusDesc,removeNumbers)
library(SnowballC)
CorpusDesc<-tm_map(CorpusDesc,stemDocument)
inspect(CorpusDesc[1:3])
CorpusDesc<-Corpus(VectorSource(CorpusDesc))
dtm = DocumentTermMatrix(CorpusDesc,control = list(removePunctuation=TRUE,removeWords=TRUE))
sparse<-removeSparseTerms(dtm,0.99)
DescWords<-as.data.frame(as.matrix(sparse))
colnames(DescWords)<-make.names(colnames(DescWords))
negwords<-c("blemish","crack","damage","dent","scratch","wear","tear","lock")
negCount<-apply(as.matrix(DescWords[,names(DescWords)%in%negwords]),1,sum)
no<-grepl("no",c(train$description,test$description))
NO<-grepl("NO",c(train$description,test$description))
No<-grepl("No",c(train$description,test$description))
WordNo<-no|NO|No
negCount<-negCount*(!WordNo)
train$negative<-negCount[1:nrow(train)]
test$negative<-negCount[(nrow(train)+1):(nrow(train)+nrow(test))]
clustdata<-rbind(train[,-c(1,10:11)],test[,-c(1,10)])
distance<-as.dist(gower.dist(clustdata))
clusteritems<-hclust(distance,"ward.D")
plot(clusteritems)
rect.hclust(clusteritems,4)
clusterGroups<-cutree(clusteritems,k=4)
trainG<-clusterGroups[1:1860]
testG<-clusterGroups[1861:2658]
train1<-train[trainG==1,]
train2<-train[trainG==2,]
train3<-train[trainG==3,]
train4<-train[trainG==4,]
test1<-test[testG==1,]
test2<-test[testG==2,]
test3<-test[testG==3,]
test4<-test[testG==4,]
Mod1<-glm(sold ~ .,data = train1[,-c(1,5,6,11)],family = binomial)
summary(Mod1)#626.25
M1<-glm(formula = sold~biddable+startprice+condition+storage+productline,family = binomial,data = train1[,-c(1,5,6,11)])
Mod2<-glm(sold ~ .,data = train2[,-c(1,5,6,11)],family = binomial)
summary(Mod2)#457.37
M2<-glm(formula = sold~biddable+startprice+condition+storage+productline,family = binomial,data = train2[,-c(1,5,6,11)])
Mod3<-glm(sold ~ .,data = train3[,-c(1,5,6,11)],family = binomial)
summary(Mod3)#265.85
M3<-glm(formula = sold~biddable+startprice+condition+storage+productline,family = binomial,data = train3[,-c(1,5,6,11)])
Mod4<-glm(sold ~ .,data = train4[,-c(1,5,6,11)],family = binomial)
summary(Mod4)#433.95
M4<-glm(formula = sold~biddable+startprice+condition+storage+productline,family = binomial,data = train4[,-c(1,5,6,11)])

p1<-predict(M1,type = "response")
p2<-predict(M2,type = "response")
p3<-predict(M3,type = "response")
p4<-predict(M4,type = "response")

sold<-c(train1$sold,train2$sold,train3$sold,train4$sold)
ROCR<-prediction(c(p1,p2,p3,p4),sold)
as.numeric(performance(ROCR,"auc")@y.values)
#0.8835274
ROCRplot<-performance(ROCR,"tpr","fpr")
plot(ROCRplot,colorsize=T)
#Making predictions on the test
predict1<-predict(M1,newdata = test1[,-c(1,5,6,10)],type = "response")
predict2<-predict(M2,newdata = test2[,-c(1,5,6,10)],type = "response")
predict3<-predict(M3,newdata = test3[,-c(1,5,6,10)],type = "response")
predict4<-predict(M4,newdata = test4[,-c(1,5,6,10)],type = "response")

ID<-c(test1$UniqueID,test2$UniqueID,test3$UniqueID,test4$UniqueID)
output<-data.frame(UniqueID=ID,Probability=c(predict1,predict2,predict3,predict4))
View(output)
write.csv(output, "Logestic_Regression.csv", row.names=FALSE)
