# 737-SVM-Code
Code for 737 Project - SVM Model

########################################################################################
#Use Logistic Regression to identify significant variables and SVM for feature selection
#######################################################################################

#remove data and values from the environment
rm(list=ls())

library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)


#load the training file
Data.Binge <- read.csv("BingeTrainData.csv", stringsAsFactors = F,header=T)
Data.Binge<-Data.Binge[-1]

Data.Binge$rfbing5<- as.factor(Data.Binge$rfbing5)

train.indices <- which(sample.split(Data.Binge$rfbing5,SplitRatio = 0.70))
sampledata= Data.Binge[train.indices, ]

## use logistic regression to get to the feature selection part

#model_1 identifies the significant variables (most asteriks = most significant = best p value = less than .05 (between 0 and .05)
model_1 = glm(rfbing5 ~ ., data =sampledata, family = "binomial")
summary(model_1)

#model 2 trim down significant variables
model_2<- stepAIC(model_1, direction="both")
summary(model_2)

#vif() is optional that will help in early models to remove items ,normally vif values greater than 4 is really bad
vif(model_2)

#model 3 focus on the significant variables
model_3<-glm(formula = rfbing5 ~ state + strwt + rawrake + wt2rake + rfhlth + 
               hcvu651 + casthm1 + asthms1 + prace1 + mrace1 + hispanc + 
               raceg21 + racegr3 + age80 + ageg + rfbmi5 + educag + 
               smoker3 + rfsmok3 + drnkwek + rfdrhv5 + frutsum + vegesum + 
               frtlt1 + veglt1 + pa300r2 + pastrng + lmtact1 + rfseat2 + 
               rfseat3, family = "binomial", data = sampledata)
summary(model_3)

#vif will give a quick idea to remove variables with high vif
vif(model_3)

#with each model, condense variables further to drill down and leave most significant variables remaining
model_4<-glm(formula = rfbing5 ~ state + strwt + rawrake + wt2rake + rfhlth + 
               hcvu651 + casthm1 + asthms1 + prace1 + mrace1 + hispanc + 
               raceg21 + age80 + ageg + rfbmi5 + educag + 
               smoker3 + rfsmok3 + drnkwek + rfdrhv5 + frutsum + vegesum + 
               frtlt1 + veglt1 + pa300r2 + pastrng + lmtact1 + rfseat2 + 
               rfseat3, family = "binomial", data = sampledata)
summary(model_4)
model_5<-glm(formula = rfbing5 ~ state + strwt + rawrake + wt2rake + rfhlth + 
               hcvu651 + casthm1 + asthms1 + hispanc + 
               raceg21 + age80 + ageg + rfbmi5 + educag + 
               smoker3 + rfsmok3 + drnkwek + rfdrhv5 + frutsum + vegesum + 
               frtlt1 + veglt1 + pa300r2 + pastrng + lmtact1 + rfseat2 + 
               rfseat3, family = "binomial", data = sampledata)
summary(model_5)
model_6<-glm(formula = rfbing5 ~ state + strwt + rawrake + wt2rake + rfhlth + 
               hcvu651 + casthm1 + asthms1 + raceg21 + age80 + ageg + rfbmi5 + 
               smoker3 + rfsmok3 + drnkwek + rfdrhv5 + frutsum + vegesum + 
               frtlt1 + veglt1 + pa300r2 + pastrng + lmtact1 + rfseat2 + 
               rfseat3, family = "binomial", data = sampledata)
model_7<-glm(formula = rfbing5 ~ state + strwt + rawrake + wt2rake + rfhlth + 
               hcvu651 + casthm1 + asthms1 + raceg21 + age80 + ageg + rfbmi5 + 
               smoker3 + rfsmok3 + drnkwek + rfdrhv5 + frutsum + vegesum + 
               frtlt1 + veglt1 + pa300r2 + lmtact1 + rfseat2 + 
               rfseat3, family = "binomial", data = sampledata)
summary(model_7)
model_8<-glm(formula = rfbing5 ~ state + strwt + rawrake + wt2rake + rfhlth + 
               hcvu651 + casthm1 + asthms1 + raceg21 + age80 + ageg + rfbmi5 + 
               smoker3 + rfsmok3 + drnkwek + rfdrhv5 + vegesum + 
               frtlt1 + veglt1 + pa300r2 + lmtact1 + rfseat2 + 
               rfseat3, family = "binomial", data = sampledata)
summary(model_8)
model_9<-glm(formula = rfbing5 ~ state + strwt + rawrake + wt2rake + rfhlth + 
               hcvu651 +  asthms1 + raceg21 + age80 + ageg + rfbmi5 + 
               smoker3 + rfsmok3 + drnkwek + rfdrhv5 + vegesum + 
               frtlt1 + veglt1 + pa300r2 + lmtact1 + rfseat2 + 
               rfseat3, family = "binomial", data = sampledata)
summary(model_9)
model_10<-glm(formula = rfbing5 ~ state + rfhlth + 
                hcvu651 +  asthms1 + raceg21 + age80 + ageg + rfbmi5 + 
                smoker3 + rfsmok3 + drnkwek + rfdrhv5 + vegesum + 
                frtlt1 + pa300r2 + lmtact1 + rfseat2 + 
                rfseat3, family = "binomial", data = sampledata)
summary(model_10)
model_11<-glm(formula = rfbing5 ~ state + 
                hcvu651 +  asthms1 + raceg21 + age80 + ageg + rfbmi5 + 
                smoker3 + rfsmok3 + drnkwek + rfdrhv5 + vegesum + 
                frtlt1 + pa300r2 + lmtact1 + rfseat2 + 
                rfseat3, family = "binomial", data = sampledata)
summary(model_11)
model_12<-glm(formula = rfbing5 ~ state + 
                hcvu651 +  asthms1 + raceg21 + age80 + ageg + rfbmi5 + 
                smoker3 + rfsmok3 + drnkwek + rfdrhv5  + 
                frtlt1 + pa300r2 + lmtact1 + rfseat2 + 
                rfseat3, family = "binomial", data = sampledata)
summary(model_12)
model_13<-glm(formula = rfbing5 ~ hcvu651 +  asthms1 + raceg21 + age80 + rfbmi5 + 
                smoker3 + rfsmok3 + drnkwek + rfdrhv5  + 
                frtlt1 + pa300r2 + lmtact1 + rfseat2 + 
                rfseat3, family = "binomial", data = sampledata)
summary(model_13)

model_14<-glm(formula = rfbing5 ~ hcvu651 + raceg21 + age80 + rfbmi5 + 
                smoker3 + rfsmok3 + drnkwek + rfdrhv5  + 
                frtlt1 + pa300r2 + lmtact1 +  rfseat3, family = "binomial", data = sampledata)
summary(model_14)

Final_model<-model_14
library(caret)
library(e1071) 

##Checking with SVM; use SVm (rfe for feature selection)

#data frame/values of the final variables in model 14
my_attributes <- c("rfbing5","hcvu651", "raceg21", "age80","rfbmi5","smoker3","rfsmok3","drnkwek","rfdrhv5","frtlt1","pa300r2","lmtact1","rfseat3")

#saving the final model 14 variables as svm_data
svm_data<- sampledata[my_attributes]

#creates a data frame for svm_data
str(svm_data)

#makes the characters in svm_data all numeric values
svm_data$rfbing5<-as.numeric(as.character(svm_data$rfbing5))

#we are giving control variables used in rfecontrol parameter ,to give how many crossvalidation (CV) steps we need and the method specified as CV is cross validation
control <- rfeControl(functions=caretFuncs, method="cv", number=2)

results <- rfe(svm_data[,-1], svm_data$rfbing5, sizes=c(1,2), rfeControl=control , method="svmRadial")
print(results)

##################################################################################
#Predict using SVM
##################################################################################

# Model Building for BingeData - Binge drinkers c

# Data Understanding: 
# Number of Instances: 37135


#3. Data Preparation: 

setwd("F:/Chegg/Alex")
#Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)
library("dplyr")
library("ggplot2")
library("gridExtra")
library(caTools)
library(MASS)
library(car)

#Loading Data

Data.train <- read.csv("BingeTrainData.csv", stringsAsFactors = F,header=T)
test <- read.csv("BingeTestData.csv",stringsAsFactors = F,header=T)
#clean the data
colnames(Data.train)
Data.train<-Data.train[-1]
test<-test[-1]

#taking the significant variables from model 14
my_attributes <- c("rfbing5","hcvu651", "raceg21", "age80","rfbmi5","smoker3","rfsmok3","drnkwek","rfdrhv5","frtlt1","pa300r2","lmtact1","rfseat3")
Data.train<- Data.train[my_attributes]
test<-test[my_attributes]

#retrieving column rfbing5 from data to variable
resTable <- table(Data.train$rfbing5)
plot <- plot(resTable, main = "Total Number of  Binge drinkers  (Training Set)", ylab = "Number")

# Split the data into train and test set


dim(Data.train)

#Structure of the dataset
str(Data.train)

#printing first few rows
head(Data.train)

#Exploring the data
summary(Data.train)

#checking missing value
sapply(Data.train, function(x) sum(is.na(x)))


#Making our target class to factor
#labels need to be changed to factors before building the model so it will count as different levels
#with 2 values, can classify as 1 or 2
#check the differences by doing str(Data.train) before and after factorising
#several attributes influence rfbing5
#some data classifies itself as class 1 or class 2 
#from big dataset, difficult to see which combinations make this classification
#need to model a classifier based on existing data and test on data we extracted from existing data
#evaluate using test data to usee how good the model is we built
Data.train$rfbing5<- as.factor(Data.train$rfbing5)

test$rfbing5<-as.factor(test$rfbing5)

#Scaling
#set seed is used to take the dataset from same section so that whenever we run the train set won't change
set.seed(1)

#splits train data into smaller sample (70% of train data) so that SVM will work faster (20-30 min); taking the whole data will take hours to model or you need a high speed computer. You can use whole data for regression models, but for SVM models, need to use a sample of data
train.indices <- which(sample.split(Data.train$rfbing5,SplitRatio = 0.70))

#creates a dataset using the data you just split into train indices
train = Data.train[train.indices, ]


set.seed(1)

#split test data into smaller sample (30% of test data)
test.indices <- which(sample.split(test$rfbing5,SplitRatio = 0.30))
test = test[test.indices, ]

####----------------------------------------------------------------

#Constructing Model

#now that you have prepared and split your data, you can build your first model

#Using Linear Kernel

Model_linear <- ksvm(rfbing5~ ., data =train, scale = FALSE, kernel = "vanilladot")
#evaluate model with the test data
Eval_linear<- predict(Model_linear,test)

#confusion matrix - Linear 
#take the confusion matrix from the evaluation of the model with the test data
#confusion matrix = how accurately the model works with the new data
confusionMatrix(Eval_linear,test$rfbing5)



############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 

#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=2)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(1)
#to fit the model we need to give sigma values and cost values
#it can be anywhere between that range so we calculate which one is best using the plot
#see pdf, apply the best value for the final fit
#look at gamma and cost value for the final model
#this is only used for non linear
#for linear you use 
#grid <- expand.grid(.sigma=c(1.63300294158967e-07,2.63300294158967e-07,3.54300294158967e-07, 4.63300294158967e-07), .C=c(0.5,1,2) )
grid <- expand.grid(C=seq(1, 5, by=1))

fit.svm <- train(rfbing5~., data=train, method="svmLinear", metric=metric, tuneGrid=grid, trControl=trainControl)
print(fit.svm)
evaluate_linear_test<- predict(fit.svm,test)
confusionMatrix(evaluate_linear_test,test$rfbing5)

plot(fit.svm)

Final_model <- ksvm(rfbing5~ ., data = train, scale = FALSE, gamma=4.63300294158967e-07,cost=0.5 ,kernel = "rbfdot")
Eval_Final_model<- predict(Final_model,test)

confusionMatrix(Eval_Final_model,test$rfbing5)


test1<-read.csv("BingeTestData.csv",stringsAsFactors = F,header=T)
my_attributes2 <- c("hcvu651", "raceg21", "age80","rfbmi5","smoker3","rfsmok3","drnkwek","rfdrhv5","frtlt1","pa300r2","lmtact1","rfseat3")
test1<- test1[my_attributes2]

test1$rfbing5<-predict(Final_model,newdata=test1,type = "response")

