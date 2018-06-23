
rm(list=ls())
setwd("D:/Course Material/CUTe/CUTe2")
#load(".RData")
population_train_data <- read.csv("train_data.csv",header = TRUE)
population_test_data <- read.csv("test_data.csv",header = TRUE)

#convert blank data to NA
population_train_data[population_train_data==""] <- NA
population_test_data[population_test_data==""] <- NA

#checking train data
str(population_train_data)
colSums(is.na(population_train_data))
apply(population_train_data, 2, function(col) round(sum(is.na(col))/length(col)*100,2))


#Checking teast data
nrow(population_test_data)
str(population_test_data)
colSums(is.na(population_test_data))
apply(population_test_data, 2, function(col) round(sum(is.na(col))/length(col)*100,2))

summary(population_train_data)
summary(population_test_data)

#------------------------------------------------------------------------------------
#remove index, tax_paid from both test and train data
#remove 'years_of_education' from data as it is similar to qualification after factorising, factor with 16 levels
#remove 'loan_taken' & 'financial_weight' as suggested by anova over base model
population_train_data <- population_train_data[,!names(population_train_data)%in% 
                         c("tax_paid","index","years_of_education","financial_weight","loan_taken")]
index=population_test_data$index
population_test_data <- population_test_data[,!names(population_test_data)%in% 
                         c("tax_paid","index","years_of_education","financial_weight","loan_taken")]
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
#-----------------------------------------hist(population_train_data$age)
#bin age to 4 categories "young","Adult","MiddleAged","Old"
agebin_tr <- cut(population_train_data$age,c(0,25.1,45.1,65.1,100), 
             labels=c("young","Adult","MiddleAged","Old"))
#Replacing column with binned values
population_train_data[,"age"] <- agebin_tr
str(population_train_data$age)
agebin_tst <- cut(population_test_data$age,c(0,25.1,45.1,65.1,100), 
                 labels=c("young","Adult","MiddleAged","Old"))
population_test_data[,"age"] <- agebin_tst
str(population_test_data$age)


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
#-------------------------------------------------------------------------------------------


#remove NA values
population_train_data <- na.omit(population_train_data)
colSums(is.na(population_train_data))
nrow(population_train_data)


#create train-test split using 'createDataPartition' from caret pkg- 70%train ,30%- validate
set.seed(123)
train_rows <- caret::createDataPartition(population_train_data$target, p = 0.7, list = F)
train_data <- population_train_data[train_rows, ]
validate_data <- population_train_data[-train_rows, ]
length(train_data$age)
length(validate_data$age)


str(train_data)
#building a  model
my_model <- glm(target~., data = train_data, family = binomial)
#display the results of our model
summary(my_model)
#AIC is 13977
anova(my_model, test="Chisq")
# from anova model - 'loan_taken' & 'financial_weight' can be avoided


#get predictions
prob_train <- predict(object=my_model,newdata = train_data, type = "response")
pred <- ROCR::prediction(prob_train, train_data$target)


#-------for testing only------------------
#confusion matrix for 
x=table(train_data$target,prob_train>0.52)
percentage_wrong=((as.vector(x[2])+as.vector(x[3]))/sum(as.vector(x)))*100
print(percentage_wrong)
#---------------------------------------

#Extract performance measures (True Positive Rate and False Positive Rate) 
#using the "performance()" function from the ROCR package
perf <- ROCR::performance(pred, measure="tpr", x.measure="fpr")


#Plot the ROC curve using the extracted performance measures (TPR and FPR)
ROCR::plot(perf, col=rainbow(10), colorize=T, print.cutoffs.at=seq(0,1,0.05))
#printing Area under the curve
perf_auc <- ROCR::performance(pred, measure="auc")
print(perf_auc@y.values[[1]])



#cutoff value of 0.29, predict the class labels on the test data using our model
prob_validate <- predict(my_model, validate_data, type = "response")
preds_validate <- ifelse(prob_validate > 0.29, "1", "0")

# confusion matrix
caret::confusionMatrix(preds_validate, validate_data$target, positive = "1")

#-----------------------------------------------
prob_test <- predict(my_model, population_test_data, type = "response")
preds_test <- ifelse(prob_test > 0.29, "1", "0")


submitdata <- data.frame("index" = index, "target" = preds_test)
head(submitdata)
write.csv(submitdata, "D:/Course Material/CUTe/CUTe2/SampleSubmission.csv", row.names=F)

#81.36


