
install.packages(c("dplyr", "lubridate", "tidyr"))
used_car_data <- read.csv("used_car_dataset.csv", na.strings="")
library(dplyr)
library(lubridate)
library(tidyr)
DF<-data.frame()
View(used_car_data)
dim(used_car_data)
names(used_car_data)

colSums(is.na(used_car_data))

KmDriven_counts <- table(used_car_data$kmDriven)

sorted_kmDriven_counts <- sort(KmDriven_counts, decreasing = TRUE)
most_frequent_kmDriven <- names(sorted_kmDriven_counts)[1]

print(most_frequent_kmDriven)

used_car_data <- na.omit(used_car_data)

colSums(is.na(used_car_data))
#used_car_data$PostedDate <- dmy(used_car_data$PostedDate)

 
  used_car_data <- used_car_data[, -3]
 
  used_car_data$Age[used_car_data$Age<0]<-used_car_data$Age[used_car_data$Age<0]*(-1)
  
  used_car_data <- used_car_data[, -6]
  
  used_car_data <- used_car_data[, -8]
  
  #used_car_data$KmDriven <- as.character(used_car_data$KmDriven)
  
  #.str.replace(' km', '').str.replace(',', '').astype(float)

  used_car_data$kmDriven <- as.numeric(gsub("[ km,]", "", used_car_data$kmDriven))
  
  
  used_car_data$AskPrice <- as.numeric(gsub('[₹,]', '', used_car_data$AskPrice))
  
  used_car_data$Brand <- as.factor(used_car_data$Brand)
  used_car_data$Brand <- as.numeric(used_car_data$Brand)
  
  used_car_data$model <- as.factor(used_car_data$model)
  used_car_data$model <- as.numeric(used_car_data$model)
  
  used_car_data$kmDriven <- as.factor(used_car_data$kmDriven)
  used_car_data$kmDriven <- as.numeric(used_car_data$kmDriven)
  
  used_car_data$Transmission <- as.factor(used_car_data$Transmission)
  used_car_data$Transmission <- as.numeric(used_car_data$Transmission)
  
  used_car_data$FuelType <- as.factor(used_car_data$FuelType)
  used_car_data$FuelType <- as.numeric(used_car_data$FuelType)
  used_car_data$AskPrice <- as.factor(used_car_data$AskPrice)
  used_car_data$AskPrice <- as.numeric(used_car_data$AskPrice)
  

  
  used_car_data <- used_car_data[, -7]
  str(used_car_data)
  par(mfrow=c(2,3))
  
  plot(sort(used_car_data$Brand))
  plot(used_car_data$Brand, ylab = "Brand", xlab = "Rank", main = "Distribution of Brand")
  

  plot(sort(used_car_data$model))
  plot(used_car_data$model, ylab = "model", xlab = "Rank", main = "Distribution of model")
  
  
  plot(sort(used_car_data$Age))
  plot(used_car_data$Age, ylab = "Age", xlab = "Rank", main = "Distribution of Age")
  
  
  plot(sort(used_car_data$kmDriven))
  plot(used_car_data$kmDriven, ylab = "kmDriven", xlab = "Rank", main = "Distribution of kmDriven")
  
 
  plot(sort(used_car_data$Transmission))
  plot(used_car_data$Transmission, ylab = "Transmission", xlab = "Rank", main = "Distribution of Transmission")
  

  plot(sort(used_car_data$FuelType))
  plot(used_car_data$FuelType, ylab = "FuelType", xlab = "Rank", main = "Distribution of FuelType")
  

  plot(sort(used_car_data$AskPrice))
  plot(used_car_data$AskPrice, ylab = "AskPrice", xlab = "Rank", main = "Distribution of AskPrice")
  
  plot(used_car_data)
  
  
  plot(used_car_data$Brand, used_car_data$AskPrice, 
       xlab = "Brand", ylab = "AskPrice", 
       main = "Type of Brand Effect on AskPrice", 
       col = "blue")
  
  plot(used_car_data$model, used_car_data$AskPrice, 
       xlab = "model", ylab = "AskPrice", 
       main = "Type of model Effect on AskPrice", 
       col = "blue")
  
  plot(used_car_data$Age, used_car_data$AskPrice, 
       xlab = "Age", ylab = "AskPrice", 
       main = "Type of Age Effect on AskPrice", 
       col = "blue")
  
  plot(used_car_data$kmDriven, used_car_data$AskPrice, 
       xlab = "kmDriven", ylab = "AskPrice", 
       main = "Type of kmDriven Effect on AskPrice", 
       col = "blue")
  
  
  plot(used_car_data$Transmission, used_car_data$AskPrice, 
       xlab = "Transmission", ylab = "AskPrice", 
       main = "Type of Transmission Effect on AskPrice", 
       col = "blue")
  
  
  plot(used_car_data$FuelType, used_car_data$AskPrice, 
       xlab = "FuelType", ylab = "AskPrice", 
       main = "Type of FuelType Effect on AskPrice", 
       col = "blue")
  

  hist(used_car_data$Brand, col = "lightblue", ylab="frequency" ,xlab = "Brand", main = "Brand Histogram")
  
  hist(used_car_data$model, col = "lightblue", ylab="frequency" ,xlab = "model", main = "model Histogram")
  
  hist(used_car_data$Age, col = "lightblue", ylab="frequency" ,xlab = "Age", main = "Age Histogram")
  
  hist(used_car_data$kmDriven, col = "lightblue", ylab="frequency" ,xlab = "kmDriven", main = "kmDriven Histogram") 
  
  hist(used_car_data$Transmission, col = "lightblue", ylab="frequency" ,xlab = "Transmission", main = "Transmission Histogram")
  
  hist(used_car_data$FuelType, col = "lightblue", ylab="frequency" ,xlab = "FuelType", main = "FuelType Histogram")
  
  hist(used_car_data$AskPrice, col = "lightblue", ylab="frequency" ,xlab = "AskPrice", main = "AskPrice Histogram")
  

    boxplot(used_car_data$Brand, col = rainbow(6), ylab = "Brand Boxplot")
  rug(used_car_data$Brand,side=2)

  
    boxplot(used_car_data$model, col = rainbow(6), ylab = "model Boxplot")
  rug(used_car_data$model,side=2)

  
#__founding outliers in Age
  dev.new()
    boxplot(used_car_data$Age, col = rainbow(6), ylab = "Age Boxplot")
  rug(used_car_data$Age,side=2)
  
age_out_rm <- boxplot.stats(used_car_data$Age)$out
out_in <- which(used_car_data$Age %in% c(age_out_rm))

used_car_data<-used_car_data[-out_in,] # remove outliers


dev.new()
boxplot(used_car_data[-out_in,]$Age, col = rainbow(6), ylab = "Age Boxplot")
rug(used_car_data$Age,side=2)

#____________________________

  boxplot(used_car_data$kmDriven, col = rainbow(6), ylab = "kmDriven Boxplot")
  rug(used_car_data$kmDriven,side=2)
  

  boxplot(used_car_data$Transmission, col = rainbow(6), ylab = "Transmission Boxplot")
  rug(used_car_data$Transmission,side=2)

  boxplot(used_car_data$FuelType, col = rainbow(6), ylab = "FuelType Boxplot")
  rug(used_car_data$FuelType,side=2)

  boxplot(used_car_data$AskPrice, col = rainbow(6), ylab = "AskPrice Boxplot")
  rug(used_car_data$AskPrice,side=2)

  boxplot(used_car_data, col = rainbow(6), ylab = "used_car_data Boxplot")
  

#_______________MODELS________________#
  
  # needed packages
  install.packages('caret', dependencies = TRUE)
  install.packages("xgboost")
  install.packages("caret")
  library(caret)
  
# 1. Linear Regression
  
#split data
trainIndex <- createDataPartition(used_car_data$AskPrice, p = 0.8, list = FALSE)
trainData <- used_car_data[trainIndex, ]
testData <- used_car_data[-trainIndex, ]

dim(trainIndex)
dim(trainData)
dim(testData)
 
# training model 
linear_model_cars <- lm(AskPrice ~. , data = trainData)

str(linear_model_cars)

# calculate rmse for trained data
used_car_reg_rmse_train <- trainData %>%
  mutate(pred.reg.train = predict(linear_model_cars))

View(used_car_reg_rmse_train)

dev.new()
plot(used_car_reg_rmse_train$AskPrice, used_car_reg_rmse_train$pred.reg.train)
abline(0, 1, col = "red")

mse <- used_car_reg_rmse_train %>%
  mutate(error = pred.reg.train - AskPrice,
         sq.error = error^2) %>%
  summarise(mse = mean(sq.error))

View(mse)

rmse_train<-sqrt(mse)
print(rmse_train)

#calculate rmse for test data

pred_reg_test <- predict(linear_model_cars, newdata = testData)
reg_rmse_test <- sqrt(mean((pred_reg_test - testData$AskPrice)^2))

r_squared <- cor(testData$AskPrice, pred_reg_test)^2
print(r_squared)
print(reg_rmse_test)

#______________________________________________

