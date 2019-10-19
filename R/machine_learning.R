#install.packages("randomForest")
library(randomForest)
library(dplyr)

patients = select(patients, 2, 4:15)
head(patients)

str(patients)

summary(patients)


set.seed(100)

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
train <- sample(nrow(patients), 0.7*nrow(patients), replace = FALSE)
TrainSet <- patients[train,]
ValidSet <- patients[-train,]
summary(TrainSet)
summary(ValidSet)

model1 <- randomForest(disease ~ ., data = TrainSet, importance = TRUE)
model1

# Fine tuning parameters of Random Forest model
model2 <- randomForest(disease ~ ., data = TrainSet, ntree = 500, mtry = 13, importance = TRUE)
model2

# Predicting on train set
predTrain <- predict(model2, TrainSet, type = "class")

# Checking classification accuracy
table(predTrain, TrainSet$disease) 
#model2 <- na.omit(model2) 

# Predicting on Validation set
predValid <- predict(model2, ValidSet, type = "class")

# Checking classification accuracy
mean(predValid == ValidSet$disease)                    
table(predValid,ValidSet$disease)


# To check important variables
importance(model2)        
varImpPlot(model2)     

# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(disease ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = "class")
  a[i-2] = mean(predValid == ValidSet$disease)
}

a

plot(3:8,a)

# Compare with Decision Tree
model_dt = train(disease ~ ., data = TrainSet, method = "rpart")
model_dt_1 = predict(model_dt, data = TrainSet)
table(model_dt_1, TrainSet$disease)

mean(model_dt_1 == TrainSet$disease)

# Running on Validation Set
model_dt_vs = predict(model_dt, newdata = ValidSet)
table(model_dt_vs, ValidSet$disease)

mean(model_dt_vs == ValidSet$disease)







