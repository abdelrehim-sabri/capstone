set.seed(1234)
options(warm=-1)
library(lubridate)
library(ggplot2)
library(randomForest)
library(dplyr)
library(rpart)
library(caret)
library(caretEnsemble)
library(e1071)
library(corrplot)
library(unbalanced)
library("mlbench")
library("pROC")

build_clean_dataset <- function() {
  datasetloc = "C:/Users/abdel/Desktop/Ryerson University/capstone/capstone/R/Health_Care_History.csv"
  if (file.exists(datasetloc)) {
    alldata <- read.csv(file=datasetloc, header = T)
  }
  return(alldata)
}

age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  
  return(calc.age)
}

get_age_group <- function(a) {
  ifelse(a<25,25, ifelse(a<40, 40, ifelse(a<50,50,65)))
}

east_europe <- c('Ukraine','Russia','Poland','Czech Republic','Hungary')
west_europe <- c('Austria','Belgium','France','Germany','Italy','Netherlands','Portugal','Spain','Switzerland')
north_europe <- c('Sweden', 'Finland', 'Denmark')
british <- c('England','Scotland','Ireland')

get_ethnic_group <- function(country) {
  ifelse((country %in% east_europe), 'east_europe',
         ifelse((country %in% west_europe) ,'west_europe',
                ifelse((country %in% north_europe), 'north_europe',
                       ifelse((country %in% british), 'british',
                              country))))
}

patients <- build_clean_dataset()

#remove the patient ids from the dataset
patients <- patients[,-1]
str(patients)

install.packages("psych")
library(psych)

tbl <- table(patients$gender,patients$dob,patients$zipcode,patients$employment_status,patients$education,
             patients$marital_status,patients$children,patients$ancestry,patients$avg_commute,patients$daily_internet_use,
             patients$available_vehicles,patients$military_service,patients$disease)

tbl <- table(patients$gender,patients$dob,patients$zipcode,patients$employment_status,patients$education)
chisq.test(tbl) 


cor(patients[,-12])



summary(patients)

patients$education <- ifelse(patients$education == 'highscool', as.character('highschool'), as.character(patients$education))
patients$education <- ifelse(as.factor(patients$education) == 'phD/MD', as.character('phd/md'), as.character(patients$education))
patients$education <- as.factor(patients$education)

patients$ancestry <- as.factor(get_ethnic_group(patients$ancestry))

patients$age <- age(patients$dob)

get_binary_value <- function(value, compare_to) {
  ifelse(value==compare_to,1,0)
}
patients$prostate_cancer <- get_binary_value(patients$disease,'prostate cancer')
patients$skin_cancer <- get_binary_value(patients$disease,'skin cancer')
patients$breast_cancer <- get_binary_value(patients$disease,'breast cancer')
patients$hiv_aids <- get_binary_value(patients$disease,'HIV/AIDS')
patients$diabetes <- get_binary_value(patients$disease,'diabetes')
patients$heart_disease <- get_binary_value(patients$disease,'heart disease')
patients$hypertension <- get_binary_value(patients$disease,'hypertension')
patients$endometriosis <- get_binary_value(patients$disease,'endometriosis')
patients$multiple_sclerosis <- get_binary_value(patients$disease,'multiple sclerosis')
patients$schizophrenia <- get_binary_value(patients$disease,'schizophrenia')
patients$kidney_disease <- get_binary_value(patients$disease,'kidney disease')
patients$gastritis <- get_binary_value(patients$disease,'gastritis')
patients$alzheimer <- get_binary_value(patients$disease,'Alzheimer disease')
str(patients)


train <- sample(nrow(os_alzheimer), 0.7*nrow(os_alzheimer), replace = FALSE)
TrainSet <- os_alzheimer[train,]
ValidSet <- os_alzheimer[-train,]
summary(TrainSet)
summary(ValidSet)

response <- as.factor(TrainSet$alzheimer)
input <- select(TrainSet, gender, age, employment_status, education, marital_status, ancestry)

data <- ubUnder(X=input, Y=response, perc=40, method="percPos")
us_alzheimer <- cbind(data$X, class=data$Y)


barplot(table(us_alzheimer[,7]), xlab=colnames(us_alzheimer)[7])

data <- ubOver(X=input, Y=response)
os_alzheimer <- cbind(data$X, class=data$Y)

barplot(table(os_alzheimer[,7]), xlab=colnames(os_alzheimer)[7])

data <- ubSMOTE(X=input, Y=response)
smote_alzheimer <- cbind(data$X, class=data$Y)

barplot(table(smote_alzheimer[,7]), xlab=colnames(smote_alzheimer)[7])


train_control <- trainControl(method = "repeatedcv", number = 10, repeats=3, savePredictions = TRUE)
us_glm_model <- caret::train(class~.,data=us_alzheimer, trControl = train_control, method="glm", family="binomial", tuneLength = 5)
summary(glm_model)
os_glm_model <- caret::train(class~.,data=os_alzheimer, trControl = train_control, method="glm", family="binomial", tuneLength = 5)
summary(glm_model)
smote_glm_model <- caret::train(class~.,data=smote_alzheimer, trControl = train_control, method="glm", family="binomial", tuneLength = 5)
summary(glm_model)
pred = predict(glm_fit, newdata=ValidSet)
confusionMatrix(data=pred, ValidSet$class)
