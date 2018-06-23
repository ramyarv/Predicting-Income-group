
rm(list=ls())
setwd("C:\\Users\\rvadamala\\Documents\\insofe\\CUTE_CSE7302C")
#load(".RData")
population_train_data <- read.csv("train_data.csv",header = TRUE,na.strings = c("NA",""))
population_test_data <- read.csv("test_data.csv",header = TRUE,na.strings = c("NA",""))

#checking train data
str(population_train_data)




summary(population_train_data)
colSums(is.na(population_train_data))
apply(population_train_data, 2, function(col) round(sum(is.na(col))/length(col)*100,2))


#Checking teast data
nrow(population_test_data)
summary(population_test_data)

str(population_test_data)
colSums(is.na(population_test_data))
apply(population_test_data, 2, function(col) round(sum(is.na(col))/length(col)*100,2))




#------------------------------------------------------------------------------------
#remove index, tax_paid from both test and train data as index is unique value and 
#taxpaid has 92.46% & 82.38% NA Values on train and test 
#remove 'years_of_education' from data as it is similar to qualification after factorising, factor with 16 levels
#remove 'loan_taken' & 'financial_weight' as suggested by anova over base model
population_train_data <- population_train_data[,!names(population_train_data)%in% 
                         c("tax_paid","index","years_of_education")]
index=population_test_data$index
population_test_data <- population_test_data[,!names(population_test_data)%in% 
                         c("tax_paid","index","years_of_education")]
colnames(population_train_data)
colnames(population_test_data)


#factorize some colums which currently are numerical      
columns_to_factorize <- c("target")
population_train_data[columns_to_factorize] <- lapply(population_train_data[columns_to_factorize],factor)
str(population_train_data$target)




#delete the rows with NA values in age in train data, as most other values on that row are NA
summary(population_train_data$age)
summary(population_test_data$age)#on test_data there are no NA values
population_train_data=population_train_data[!is.na(population_train_data$age),]
sum(is.na(population_train_data$age))


# marital status
levels(population_train_data$marital_status)

maritalStatus <- plyr::revalue(population_train_data$marital_status,c("Divorced" = "Separated",
                                                                      "Married-civilian" ="Married",
                                                                      "Married-defence" ="Married",
                                                                      "Married-non-resident"="Married"))
population_train_data[,"marital_status"] <- maritalStatus

maritalStatusTest <- plyr::revalue(population_test_data$marital_status,c("Divorced" = "Separated",
                                                                      "Married-civilian" ="Married",
                                                                      "Married-defence" ="Married",
                                                                      "Married-non-resident"="Married"))
population_test_data[,"marital_status"] <- maritalStatusTest

levels(population_test_data$marital_status)

#---------------------Handling Na Values-------------------------------------------

colSums(is.na(population_train_data))
colSums(is.na(population_test_data))

levels(population_train_data$working_sector) <- c(levels(population_train_data$working_sector), "missing")
population_train_data$working_sector[is.na(population_train_data$working_sector)] <- "missing"

levels(population_train_data$occupation) <- c(levels(population_train_data$occupation), "missing")
population_train_data$occupation[is.na(population_train_data$occupation)] <- "missing"

levels(population_train_data$country) <- c(levels(population_train_data$country), "missing")
population_train_data$country[is.na(population_train_data$country)] <- "missing"

levels(population_test_data$working_sector) <- c(levels(population_test_data$working_sector), "missing")
population_test_data$working_sector[is.na(population_test_data$working_sector)] <- "missing"

levels(population_test_data$occupation) <- c(levels(population_test_data$occupation), "missing")
population_test_data$occupation[is.na(population_test_data$occupation)] <- "missing"

levels(population_test_data$country) <- c(levels(population_test_data$country), "missing")
population_test_data$country[is.na(population_test_data$country)] <- "missing"


#impute NA values usind central imputation
population_train_data <- DMwR::centralImputation(population_train_data)
colSums(is.na(population_train_data))
population_test_data <- DMwR::centralImputation(population_test_data)
colSums(is.na(population_test_data))
colSums(is.na(population_train_data))
colSums(is.na(population_test_data))


#make a new variable netgain = gain- loss
#bin netgain to prop.table(mytable, 1)
 
plot(table(population_train_data$netgain))
population_train_data$netgain <- population_train_data$gain-population_train_data$loss
population_test_data$netgain <- population_test_data$gain- population_test_data$loss
population_train_data$gain <- NULL
population_train_data$loss <- NULL
population_test_data$gain <- NULL
population_test_data$loss <- NULL


# Dividing countries continent wise
#-----------------------------plot(population_train_data$country,population_train_data$target)
sort(summary(population_train_data$country),decreasing = T)
levels(population_train_data$country)
cntry_tr <- plyr::revalue(population_train_data$country,c(" United-States" = "NAm"," Cuba" =  "SAm",
                                                       " Jamaica" =  "SAm"," India" =  "Asia",
                                                       " Mexico" =  "NAm"," South" =  "NAm",
                                                       " Puerto-Rico" =  "SAm"," Honduras" = "SAm",
                                                       " England" = "Eur"," Canada" = "NAm",
                                                       " Germany" = "Eur"," Iran" = "Asia",
                                                       " Philippines" = "Asia"," Italy" = "Eur",
                                                       " Poland" = "Eur"," Columbia" = "SAm",
                                                       " Cambodia" = "Asia"," Thailand" = "Asia",
                                                       " Ecuador" = "SAm"," Laos" = "Asia",
                                                       " Taiwan" = "Asia"," Haiti" = "SAm",
                                                       " Portugal" = "Eur"," Dominican-Republic" = "SAm",
                                                       " El-Salvador" = "SAm"," France" = "Eur",
                                                       " Guatemala" = "SAm"," China" = "Asia",
                                                       " Japan" = "Asia"," Yugoslavia" = "Eur",
                                                       " Peru" = "SAm"," Outlying-US(Guam-USVI-etc)" = "NAm",
                                                       " Scotland" = "Eur"," Trinadad&Tobago" = "SAm",
                                                       " Greece" = "Eur"," Nicaragua" = "SAm",
                                                       " Vietnam" = "Asia"," Hong" = "Asia",
                                                       " Ireland" = "Eur"," Hungary" = "Eur",
                                                       " Holand-Netherlands" = "Eur" ))
population_train_data[,"country"] <- cntry_tr
str(population_train_data$country)
cntry_tst <- plyr::revalue(population_test_data$country,c(" United-States" = "NAm"," Cuba" =  "SAm",
                                                          " Jamaica" =  "SAm"," India" =  "Asia",
                                                          " Mexico" =  "NAm"," South" =  "NAm",
                                                          " Puerto-Rico" =  "SAm"," Honduras" = "SAm",
                                                          " England" = "Eur"," Canada" = "NAm",
                                                          " Germany" = "Eur"," Iran" = "Asia",
                                                          " Philippines" = "Asia"," Italy" = "Eur",
                                                          " Poland" = "Eur"," Columbia" = "SAm",
                                                          " Cambodia" = "Asia"," Thailand" = "Asia",
                                                          " Ecuador" = "SAm"," Laos" = "Asia",
                                                          " Taiwan" = "Asia"," Haiti" = "SAm",
                                                          " Portugal" = "Eur"," Dominican-Republic" = "SAm",
                                                          " El-Salvador" = "SAm"," France" = "Eur",
                                                          " Guatemala" = "SAm"," China" = "Asia",
                                                          " Japan" = "Asia"," Yugoslavia" = "Eur",
                                                          " Peru" = "SAm"," Outlying-US(Guam-USVI-etc)" = "NAm",
                                                          " Scotland" = "Eur"," Trinadad&Tobago" = "SAm",
                                                          " Greece" = "Eur"," Nicaragua" = "SAm",
                                                          " Vietnam" = "Asia"," Hong" = "Asia",
                                                          " Ireland" = "Eur"," Hungary" = "Eur",
                                                          " Holand-Netherlands" = "Eur" ))
population_test_data[,"country"] <- cntry_tst
str(population_test_data$country)




#create train-test split using 'createDataPartition' from caret pkg- 70%train ,30%- validate
set.seed(567)
train_rows <- caret::createDataPartition(population_train_data$target, p = 0.7, list = F)
train_data <- population_train_data[train_rows, ]
validate_data <- population_train_data[-train_rows, ]
summary(train_data$country)
summary(population_train_data$country)

#----standardizing numerical data---
num_Attr <- c("age","netgain","working_hours")

stdObj <- caret::preProcess(train_data[,(names(train_data) %in% num_Attr)],method = c("range"))
?caret::preProcess
train_data <- predict(stdObj,train_data)
validate_data <- predict(stdObj,validate_data)
population_test_data <- predict(stdObj,population_test_data)




#--------------------Glm Model-------------------------------------
str(train_data)
#building a  model
train_data$country <- NULL
my_model <- glm(target~., data = train_data, family = binomial)
my_model$xlevels[['country']]
levels(train_data$country)

my_model$xlevels[['occupation']] <- union(my_model$xlevels[['occupation']] , levels(validate_data$occupation))
my_model$xlevels[['working_sector']] <- union(my_model$xlevels[['working_sector']], levels(validate_data$working_sector))

#display the results of our model
summary(my_model)
#AIC is 14528

#----anova on top of glm model for feature selection
#anova(my_model, test="Chisq")
# from anova model - 'loan_taken' & 'financial_weight' can be avoided
summary(my_model)

#----------get predictions for train data for ROCc----
prob_train <- predict(object=my_model,newdata = train_data, type = "response")
pred <- ROCR::prediction(prob_train, train_data$target)
#Extract performance measures (True Positive Rate and False Positive Rate) 
#using the "performance()" function from the ROCR package
perf <- ROCR::performance(pred, measure="tpr", x.measure="fpr")
#Plot the ROC curve using the extracted performance measures (TPR and FPR)
ROCR::plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
#printing Area under the curve
perf_auc <- ROCR::performance(pred, measure="auc")
print(perf_auc@y.values[[1]])

#----prediction on validate data---------------------------------------
prob_validate <- predict(my_model, validate_data, type = "response")

# findind the threshold value for highest acuracy
acuracy_list <- data.frame()
for(i in seq(0.1,0.75,0.01))
{
  predsTest <- ifelse(prob_validate > i , 1, 0)
  confMatrix <- caret::confusionMatrix(predsTest,validate_data$target,positive = "1")
  #acuracy_list <- c(acuracy_list,confMatrix$overall[['Accuracy']])
  acuracy_list <- rbind(acuracy_list,data.frame(i,confMatrix$overall[['Accuracy']]))
}
names(acuracy_list)<- c("threshold","accuracy")
chosen_threshold<-acuracy_list$threshold[acuracy_list$accuracy==max(acuracy_list[,2])]

#prediction based on chosen threshold on validate data  
preds_validate <- ifelse(prob_validate > chosen_threshold, "1", "0")
# confusion matrix
caret::confusionMatrix(preds_validate, validate_data$target, positive = "1")



#-----------Predicting on population test data--------------------
prob_test <- predict(my_model, population_test_data, type = "response")
preds_test <- ifelse(prob_test > chosen_threshold, "1", "0")
confmatrix <- caret::confusionMatrix(preds_test, population_test_data$target, positive = "1")
print(confmatrix$overall[['Accuracy']])

#--84.53
## applying stepAIC on the current model

aicModel <- MASS::stepAIC(my_model)

#summary(train_data)
my_model <- aicModel




