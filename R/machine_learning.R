#install.packages("rpart")
#install.packages("caret")
#install.packages("e1071")
#install.packages("randomForest")
#install.packages("corrplot")


library(randomForest)
library(dplyr)
library(rpart)
library(caret)
library(e1071)
library(corrplot)

#exclude the patient id from analysis
#patients = select(patients, 2, 5:9,12:15)
#patients = select(patients, 2, 5:28)
head(patients)
str(patients)

# Convert all column types to factor
# col_names <- names(patients)
# patients[,col_names] <- lapply(patients[,col_names] , factor)
 
#col_names <- names(patients)
#col_names <- c('zipcode','age')
#patients[,col_names] <- lapply(patients[,col_names] , factor)

str(patients)

summary(patients)

# c("pearson", "kendall", "spearman")
#patients[,col_names] <- lapply(patients[,col_names] , numeric)
# find correlation between variables
#cor.test(as.numeric(patients$education), as.numeric(patients$disease), method = "spearman")
#cor.test(patients$age, as.numeric(patients$education), method = "pearson")
#cor.test(patients$age, as.numeric(patients$education), method = "kendall")
#cor.test(patients$age, as.numeric(patients$education), method = "spearman")
#cor.test(patients$education, patients$education)
#cor.test(as.numeric(patients$education), as.numeric(patients$employment_status))
#cor.test(as.numeric(patients$education), as.numeric(patients$ancestry))
#cor.test(patients$employment_status, patients$children)
#cor.test(patients$disease, patients$available_vehicles)



set.seed(100)

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
train <- sample(nrow(patients), 0.7*nrow(patients), replace = FALSE)
TrainSet <- patients[train,]
ValidSet <- patients[-train,]
summary(TrainSet)
summary(ValidSet)

model1 <- randomForest(employment_status ~ prostate_cancer+skin_cancer+breast_cancer+hiv_aids+diabetes+heart_disease+hypertension+endometriosis+multiple_sclerosis+schizophrenia+kidney_disease+gastritis+alzheimer, data = TrainSet)
model1

# Fine tuning parameters of Random Forest model
model2 <- randomForest(employment_status ~ prostate_cancer+skin_cancer+breast_cancer+hiv_aids+diabetes+heart_disease+hypertension+endometriosis+multiple_sclerosis+schizophrenia+kidney_disease+gastritis+alzheimer, data = TrainSet, ntree = 500, mtry = 3, importance = TRUE)
model2

# Predicting on train set
predTrain <- predict(model2, TrainSet, type = "class")

# Checking classification accuracy
table(predTrain, TrainSet$employment_status) 
#model2 <- na.omit(model2) 

# Predicting on Validation set
predValid <- predict(model2, ValidSet, type = "class")

# Checking classification accuracy
mean(predValid == ValidSet$employment_status)                    
table(predValid,ValidSet$employment_status)


# To check important variables
importance(model2)        
varImpPlot(model2)     

# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(employment_status ~ prostate_cancer+skin_cancer+breast_cancer+hiv_aids+diabetes+heart_disease+hypertension+endometriosis+multiple_sclerosis+schizophrenia+kidney_disease+gastritis+alzheimer, data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = "class")
  a[i-2] = mean(predValid == ValidSet$employment_status)
}

a

plot(3:8,a)

# Compare with Decision Tree
model_dt = train(employment_status ~ prostate_cancer+skin_cancer+breast_cancer+hiv_aids+diabetes+heart_disease+hypertension+endometriosis+multiple_sclerosis+schizophrenia+kidney_disease+gastritis+alzheimer, data = TrainSet, method = "rpart")
model_dt_1 = predict(model_dt, data = TrainSet)
table(model_dt_1, TrainSet$employment_status)

mean(model_dt_1 == TrainSet$employment_status)

# Running on Validation Set
model_dt_vs = predict(model_dt, newdata = ValidSet)
table(model_dt_vs, ValidSet$employment_status)

mean(model_dt_vs == ValidSet$employment_status)






# Compare with K-Nearest Neighbour (KNN)
#install.packages("pROC")
#install.packages("mlbench")
library(pROC)
library(mlbench)

trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
fit <- train(employment_status ~ prostate_cancer+skin_cancer+breast_cancer+hiv_aids+diabetes+heart_disease+hypertension+endometriosis+multiple_sclerosis+schizophrenia+kidney_disease+gastritis+alzheimer, data = TrainSet, method = "knn", tuneLength = 20, trControl = trControl)
fit
plot(fit)
varImp(fit)
pred <- predict(fit, newdata = ValidSet)
confusionMatrix(pred, ValidSet$employment_status)

