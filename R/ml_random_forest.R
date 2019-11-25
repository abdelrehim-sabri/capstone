#install.packages("randomForest")
library(randomForest)

data1 <- read.csv("C:/Users/abdel/Desktop/Ryerson University/capstone/capstone/R/car.data", header = TRUE)

head(data1)

str(data1)

summary(data1)
names(data1)[1]<-"BuyingPrice"
names(data1)[2]<-"Maintenance"
names(data1)[3]<-"NumDoors"
names(data1)[4]<-"NumPersons"
names(data1)[5]<-"BootSpace"
names(data1)[6]<-"Safety"
names(data1)[7]<-"Condition"


set.seed(100)

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
train1 <- sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
TrainSet1 <- data1[train1,]
ValidSet1 <- data1[-train1,]
summary(TrainSet1)
summary(ValidSet1)
str(TrainSet1)
model11 <- randomForest(Condition ~ ., data = TrainSet1, importance = TRUE)
model11

# Fine tuning parameters of Random Forest model
model22 <- randomForest(Condition ~ ., data = TrainSet1, ntree = 500, mtry = 6, importance = TRUE)
model22


# Predicting on train set
predTrain1 <- predict(model22, TrainSet1, type = "class")
# Checking classification accuracy
table(predTrain1, TrainSet1$Condition) 

# Predicting on Validation set
predValid1 <- predict(model22, ValidSet1, type = "class")
# Checking classification accuracy
mean(predValid1 == ValidSet1$Condition)                    
table(predValid1,ValidSet1$Condition)


# To check important variables
importance(model22)        
varImpPlot(model22)     

# Using For loop to identify the right mtry for model
a1=c()
i=5
for (i in 3:8) {
  model33 <- randomForest(Condition ~ ., data = TrainSet1, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model33, ValidSet1, type = "class")
  a[i-2] = mean(predValid1 == ValidSet1$Condition)
}

a1

plot(3:8,a1)







