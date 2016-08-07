library("Metrics")
library("Boruta")
library("randomForest")
library("rpart")
library("rpart.plot")
library("foreach")
library("clusterSim")
library("e1071")
library("psych")
library("stringr")
library("stats")
library("caret")
library("base")
########################################################################################
train<-read.csv("Train - Copy.csv",header = T)
test<-read.csv("Test - Copy.csv",header = T)
SampleSubmission<-read.csv("Sample_Submission.csv",header = T)
ID<-test[,1]
#View(train)
#View(test)
########################################################################################
#Checking Colinearity:
#pairs.panels(train)
########################################################################################
#Checking for Blank spaces,NA:
# sum(train %in% "")
# sum(train %in% " ")
# sum(train %in% "NA")
# sum(train %in% NA)

########################################################################################
# Feature Engg:
#Seperating date and time:

# for (i in 1:nrow(train)) {
#   train$date=strsplit(as.character(x)," ")[[i]][1]
#   train$time=strsplit(as.character(x)," ")[[i]][2]
# }

#Changing Date: Train Data
train[,3]<-as.character(train[,3])
train$DATE<- as.Date(train$DATE, format="%Y%m%d")
WEEK<- as.numeric( format(train$DATE+3, "%U"))
train<-cbind(train,WEEK)

#Test Data:
test[,3]<-as.character(test[,3])
test$DATE<- as.Date(test$DATE, format="%Y%m%d")
WEEK1<- as.numeric( format(test$DATE+3, "%U"))
test<-cbind(test,WEEK1)

#Changing time into morning and evening: Train Data
train[,4]<-as.numeric(train[,4])
train[,4]<-ifelse(train[,4] >= 6 & train[,4] <= 18,"AM","PM")
train[,4]<-as.factor(train[,4])

#Test Data
test[,4]<-as.numeric(test[,4])
test[,4]<-ifelse(test[,4] >= 6 & test[,4] <= 18,"AM","PM")
test[,4]<-as.factor(test[,4])

########################################################################################
# #NOrmalize:

# #Train Data
train[,6]<-data.Normalization (train[,6],type="n4",normalization="column")
train[,7]<-data.Normalization (train[,7],type="n4",normalization="column")
train[,8]<-data.Normalization (train[,8],type="n4",normalization="column")
train[,9]<-data.Normalization (train[,9],type="n1",normalization="column")

#Test Data
test[,5]<-data.Normalization (test[,5],type="n4",normalization="column")
test[,6]<-data.Normalization (test[,6],type="n4",normalization="column")
test[,7]<-data.Normalization (test[,7],type="n4",normalization="column")
test[,8]<-data.Normalization (test[,8],type="n4",normalization="column")


########################################################################################
#Remove Unwanted Columns:
train<-train[,c(-1,-2,-3)]
test<-test[,c(-1,-2,-3)]

########################################################################################
#GBM:

# fitControl <- trainControl(method = "repeatedcv", number = 7, repeats = 7)
# 
# gbmFit1 <- train(TARGETVAR~.,train, method = "gbm", trControl = fitControl,verbose = FALSE)
# 
# TARGETVAR<-predict(gbmFit1,test)

rm<-randomForest(TARGETVAR~.,train,ntree=120,importance = T)

iterations<-50
predictions<-foreach(m=1:iterations,.combine=cbind) %do%  {
  predict(rm,test)
}
TARGETVAR<-rowMeans(predictions)

########################################################################################

predictions = cbind(ID,TARGETVAR)
write.csv(predictions, 'SampleSubmission1.csv', row.names = F)


