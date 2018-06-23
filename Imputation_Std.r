## CUTE - CS7302C - Batch 7

## Problem Statement :: To classify a user as higher income or lower income

## Reading the data
## cleaning the environment before hand
rm(list=ls(all=T))
setwd("C:\\Users\\rvadamala\\Documents\\insofe\\CUTE_CSE7302C")
dir()

inputData <- read.csv("train_data.csv",header = T)
str(inputData)
summary(inputData)

graderData <- read.csv("test_data.csv",header = T)


## what can we do with data / are all columns relavant??

## 1) index is unique column, may not be useful for our predicction

rownames(inputData)<- inputData$index
inputData$index <- NULL

## gain and loss are two columns, it would be logical to have a single column
## where we can use netprofit = gain - loss

inputData$net_profit <- inputData$gain - inputData$loss
inputData$gain <- NULL
inputData$loss <- NULL
head(inputData)

graderData$net_profit <- graderData$gain - graderData$loss
graderData$gain <- NULL
graderData$loss <- NULL
head(graderData)

## there are two columns qualification (categorical), years_of_education(numerical)
## both represent same thing, the qualification seems more intuitive to have .. rather
## than years of education

inputData$years_of_education <- NULL
str(inputData)
summary(inputData)

graderData$years_of_education <- NULL
str(graderData)
summary(graderData)

## loan_taken is a categorical variable, it would be better to change to a factor
## else our model will treat it as numeric, same goes for target variable

inputData$loan_taken <- as.factor(as.character(inputData$loan_taken))
inputData$target <- as.factor(as.character(inputData$target))

graderData$loan_taken <- as.factor(as.character(graderData$loan_taken))
graderData$target <- as.factor(as.character(graderData$target))

num_Attr <- c("age","financial_weight","net_profit","working_hours")
category_Attr <- setdiff(names(inputData), num_Attr)
names(inputData)



## Are there any missing values ??

sum(is.na(inputData))
colSums(is.na(inputData))

## lets see how much of the data is missing

percentages <- (colSums(is.na(inputData))/ nrow(inputData)) * 100
print(percentages)

## lets check the missing values one by one 


## tax_paid column has more than 90% of missing values, even though it seems logical to have the data
## since its more related to income. But imputing most of that data may result in biased models, it might
## be too risky

inputData$tax_paid <- NULL
graderData$tax_paid <- NULL


inputDataImputed <- DMwR::centralImputation(inputData)
graderDataImputed <- DMwR::centralImputation(graderData)


## creating train/test split (70:30), graderData will be treated as unseen test data
set.seed(625)
trainDataRows <- caret::createDataPartition(inputDataImputed$target, p=0.7, list = F)
trainData <- inputDataImputed[trainDataRows,]
testData <- inputDataImputed[-trainDataRows,]

## standardizing
stdObj <- preProcess(trainData[,(names(trainData) %in% num_Attr)],method = c("center","scale"))
trainData <- predict(stdObj,trainData)
testData <- predict(stdObj,testData)
graderData <- predict(stdObj,graderData)


## Model building , lets try a basic glm model first
## Model 1 : glm model

LogRegBasic = glm(target ~ . , trainData, family = "binomial")
summary(LogRegBasic)

## There is a country with one data observation "Holand - Netherlands"
## If the corresponding data point is not in the train data, your model wont learn it
## and it will cause problems when model is trying to predict the test data values

## As a fix, we are combining the missing levels if any to the model levels

LogRegBasic$xlevels[['country']] <- union(LogRegBasic$xlevels[['country']], levels(testData$country))


## Initial Observations :
## 1) There is a substantial decrease in deviance with the introduction of model

probTrain <- predict(LogRegBasic, type="response")
## It created probability values for each of data points in the training 
## dataset.

library(ROCR)

predsTrain <- prediction(probTrain,trainData$target)

trainDataPerformance <- performance(prediction.obj = predsTrain, measure = "tpr", x.measure = "fpr")

plot(trainDataPerformance, col = rainbow(10),colorize = T, print.cutoffs.at=seq(0,1,0.05))

# Access the auc score from the train performance object

perf_auc <- performance(predsTrain, measure="auc")

auc <- perf_auc@y.values[[1]]

print(auc)

## what are false positivies - person of low income captured as high income
## what are false negatives - person of high income captured as low income
## here base case : high income (positive)

## which is more worrisome ?? whats the business objective of classifying people
## based on income??

probTest <- predict(LogRegBasic,testData, type="response")
cat("Threshold ","Accuracy ","Sensitivity ","Specificity","\n")
for(i in seq(0.25,0.55,0.02))
{
  predsTest <- ifelse(probTest > i , 1, 0)
  confMatrix <- confusionMatrix(predsTest,testData$target,positive = "1")
  cat(i," ",confMatrix$overall['Accuracy'],"\n")
}

## from the results, the threshold is chosen because the accuracy is the
## considered metric for this case and beyond 0.49 it is reducing , hence
## that will be our best threshold value

predsTest <- ifelse(probTest > 0.49 , 1, 0)

confusionMatrix(predsTest,testData$target,positive = "1")


probTestUpload <- predict(LogRegBasic,graderData, type="response")
predsTestUpload <- ifelse(probTestUpload > 0.49, 1,0)

output <- data.frame("index"=graderData$index, "target"=predsTestUpload)
write.csv(output, file="SampleSubmission.csv",row.names = F)










