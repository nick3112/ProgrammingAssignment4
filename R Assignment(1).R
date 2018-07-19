##R Assignment - Practical Machine Learning Week 4

#we need to use the training set to create a classificaatgion of classe and apply it to the testing set

#set up codes needed and the local space
library(caret); library(ggplot2); library("rmarkdown"); library("knitr");
setwd("C:/Nick/07 R/R User Notes/Practical Machine Learning/Assignment")

#import the two initial datasets
inSet<-read.csv("C:/Nick/07 R/R User Notes/Practical Machine Learning/Assignment/3pml-training.csv")
outSet<-read.csv("C:/Nick/07 R/R User Notes/Practical Machine Learning/Assignment/4pml-testing.csv")

#take a look at the intitial data
head(inSet)

#keep only variables that are not blank or NA
#classe is the response variable to model
#some variables are very low response and correlated with the "New_window" variable
#we can filter the records later to see if the prediction improves...
#this is since the test set does not include any of the additional data
inSet1<-inSet[,c(6:7,8:11, 37:49, 60:68,84:86,102,113:124,140,151:160)]
t_<-colnames(inSet1)
t_	#check the columns required are included

#histogram the variables and look at the output
#rotated through these to see if there was anything interesting (only made it through the first 7 :)
plot(inSet1$classe,inSet1[,3],pch=19,cex=0.5)	
qplot(inSet1$classe,inSet1[,3],data=inSet1) 

#as an example item 3 "roll_belt" shows skew in the data.  
#We need to centralise the data and scale prior to modelling

#now create the test and train sets for analysis
set.seed(32343)						#set sampling seed...
inTrain<-createDataPartition(y=inSet1$classe,p=0.75,list=FALSE)	#split the data set around 75/25
training<-inSet1[inTrain,]					#create train dataset
testing<-inSet1[-inTrain,]					#create test dataset
dim(training); dim(testing);					#assess the sample data

#filter the data for new_window to create a 'simpler' version of the data
training_filter<-training[training[,1]=="no",]
testing_filter<-testing[testing[,1]=="no",]
dim(training_filter); dim(testing_filter);

#GBM Full data: centre and scale pre-processing
mod1_full<-train(classe~.,data=training,preProcess=c("center","scale"),method="gbm")	

#version 2: filter the input data to see if this creates a difference
mod1_filter<-train(classe~.,data=training,preProcess=c("center","scale"),method="gbm")	

#create the predictions
pred1_full<-predict(testing,mod1_full$finalModel)
pred1_full<-predict(mod1_full,testing)
pred1_filter<-predict(mod1_filter,testing_filter)

#run the confusion matrix
confusionMatrix(pred1_full,testing$classe)
confusionMatrix(pred1_filter,testing_filter$classe)

#0.988 0.9875

#apply the final predictions to the test set
ANSWERS<-predict(mod1_full,outSet)
ANSWERS_filter<-predict(mod1_filter,outSet)
ANSWERS
ANSWERS_filter

#in practice the answers are the same, could have anticipated this.
#100% accuracy in the final validation/quiz set.  given 98% accuracy on a sample of 20 this was to be expected.