#install.packages("rpart")
#install.packages("caret")
#install.packages("e1071")

library(rpart)
library(caret)
library(e1071)
library(DMwR)

# We will compare model 1 of Random Forest with Decision Tree model

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10, 
                     verboseIter = FALSE,
                     sampling = "smote")

set.seed(42)
model_rf_smote <- caret::train(disease ~ .,
                               data = patients,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)

