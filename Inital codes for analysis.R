#course project : human activity recognition
library(caret)
data<-read.csv("pml-training.csv")

#Remove missing values 

colNAData<-apply(is.na(data),2,sum)
colWithNa<-colNAData[colNAData>0.5*nrow(data)]

drop<-names(colWithNa)

newData<-data[,!(names(data)%in%drop)]
newData$classe<-as.factor(newData$classe)

#filter unncessory columns in the data 
#filter character columns 
charColumnIndex<-sapply(newData, is.character)
newData<-newData[,!charColumnIndex]

#filter unncessary columns at the start of data 
#X ,raw_timestamp_part_1,raw_timestamp_part_2,num_window

newData<-subset(newData,select = -c(1:4))

#build validation data 

inBuild<-createDataPartition(y=newData$classe,p=0.7,list = FALSE)

buildData<-newData[inBuild,]
validation<-newData[-inBuild,]

#training and testing data 
inTrain<-createDataPartition(y=buildData$classe,p=0.7,list = FALSE)
training<-buildData[inTrain,]
testing<-buildData[-inTrain,]

training<-training[,-c(1:7)] #removing unnecessary data from training set 
training$classe<-as.factor(training$classe)

#modelFit<-train(classe~.,data=training,method="rf")

#check for zero variance of any predictor 
nearZeroVar(training,saveMetrics=TRUE)

#Model fits 
library(caret)
#1. Random forest 
library(randomForest)

modelFitRF<-train(classe~.,data=training,method="rf")
predRF<-predict(modelFitRF,testing)
confusionMatrix(predRF,testing$classe)

#testing data 
testData<-read.csv("pml-testing.csv")
finalnames<-names(training)
newTestData<-testData[,(names(testData)%in%finalnames)]

testPrediction<-predict(modelFitRF,newTestData)

## 
#Introduction 
#table of content 
#Data Exploration 
#Data splitting 
#Model generation
#Model Testing and Analyis 

