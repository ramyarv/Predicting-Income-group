
rm(list=ls())
setwd("D:/Course Material/CUTe/CUTe2")
#load(".RData")
population_train_data <- read.csv("train_data.csv",header = TRUE,na.strings = c("NA",""))
population_test_data <- read.csv("test_data.csv",header = TRUE,na.strings = c("NA",""))


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


#-----------------------------------------hist(population_train_data$working_hours)
#bin working_hours to 4 categories "PartTime","Regular","OverTime","ExtremelyHigh"
whrsbin_tr <- cut(population_train_data$working_hours,c(0,25.1,45.1,65.1,100), 
                  labels=c("PartTime","Regular","OverTime","ExtremelyHigh"))
population_train_data[,"working_hours"] <- whrsbin_tr
str(population_train_data$working_hours)
whrsbin_tst <- cut(population_test_data$working_hours,c(0,25.1,45.1,65.1,100), 
                  labels=c("PartTime","Regular","OverTime","ExtremelyHigh"))
population_test_data[,"working_hours"] <- whrsbin_tst
str(population_test_data$working_hours)



#---------------plot(population_train_data$loss)
#make a new variable netgain = gain- loss
#bin netgain to prop.table(mytable, 1)
 
plot(table(population_train_data$netgain))
population_train_data$netgain <- population_train_data$gain-population_train_data$loss
population_test_data$netgain <- population_test_data$gain- population_test_data$loss
population_train_data$gain <- NULL
population_train_data$loss <- NULL
population_test_data$gain <- NULL
population_test_data$loss <- NULL

netgain_bin <- cut(population_train_data$netgain,c(-Inf,-2000,-900,2000,7000,Inf), 
                   labels=c("HighLoss","MedLoss","Null","MedGain","HighGain"))
population_train_data$netgain <- netgain_bin
netgain_bin <- cut(population_test_data$netgain,c(-Inf,-2000,-900,2000,7000,Inf), 
                   labels=c("HighLoss","MedLoss","Null","MedGain","HighGain"))
population_test_data$netgain <- netgain_bin

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


#dividing qualification 
#-------------------------plot(population_train_data$qualification)
qual_tr <-plyr::revalue(population_train_data$qualification,
                        c(" Assoc-acdm" = "Assoc",
                          " Assoc-voc" = "Assoc"," 9th" = "HS"," 10th" = "HS",
                          " 11th" = "HS"," 12th" = "HS"," 5th-6th" = "UP",
                          " 7th-8th" = "UP"," 1st-4th" = "LP"," Preschool" = "LP"))
population_train_data[,"qualification"] <- qual_tr
str(population_train_data$qualification)
qual_tst <-plyr::revalue(population_test_data$qualification,
                        c(" Assoc-acdm" = "Assoc",
                          " Assoc-voc" = "Assoc"," 9th" = "HS"," 10th" = "HS",
                          " 11th" = "HS"," 12th" = "HS"," 5th-6th" = "UP",
                          " 7th-8th" = "UP"," 1st-4th" = "LP"," Preschool" = "LP"))
population_test_data[,"qualification"] <- qual_tst
str(population_test_data$qualification)


#marital_status
#------------------------------plot(population_train_data$marital_status)
mstat_tr <- plyr::revalue(population_train_data$marital_status,
                          c("Married-civilian"="Married","Married-non-resident"="Married",
                            "Married-defence"="Married"))
population_train_data[,"marital_status"] <- mstat_tr
str(population_train_data$marital_status)
mstat_tst <- plyr::revalue(population_test_data$marital_status,
                          c("Married-civilian"="Married","Married-non-resident"="Married",
                            "Married-defence"="Married"))
population_test_data[,"marital_status"] <- mstat_tst
str(population_test_data$marital_status)

#-------------------------------------------------------------------------------------------
str(population_test_data)

#impute NA values

population_train_data <- DMwR::centralImputation(population_train_data)
colSums(is.na(population_train_data))

population_test_data <- DMwR::centralImputation(population_test_data)
colSums(is.na(population_test_data))



#create train-test split using 'createDataPartition' from caret pkg- 70%train ,30%- validate
set.seed(123)
train_rows <- caret::createDataPartition(population_train_data$target, p = 0.7, list = F)
train_data <- population_train_data[train_rows, ]
validate_data <- population_train_data[-train_rows, ]
length(train_data$age)
length(validate_data$age)


#-----------------------------------------NB--------------------
library(e1071)
nb_model <- naiveBayes(target~.,data = train_data)
nb_model


#Lets test the model
nb_test_predict <- predict(nb_model,validate_data)

#fraction of correct predictions
mean(nb_test_predict==validate_data$target)

#confusion matrix
table(pred=nb_test_predict,true=validate_data$target)

#accuracy=(69+45)/(69+47+11+3)
#accuracy

#--------------------------------------------------------------------
prob_test <- predict(nb_model, population_test_data)



submitdata <- data.frame("index" = index, "target" = prob_test)
head(submitdata)
write.csv(submitdata, "D:/Course Material/CUTe/CUTe2/SampleSubmission.csv", row.names=F)

#78.89


