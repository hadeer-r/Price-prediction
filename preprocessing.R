
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

used_car_data$kmDriven[is.na(used_car_data$kmDriven)]<-most_frequent_kmDriven

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
  
  boxplot(used_car_data$Age, col = rainbow(6), ylab = "Age Boxplot")
  rug(used_car_data$Age,side=2)
  
  boxplot(used_car_data$kmDriven, col = rainbow(6), ylab = "kmDriven Boxplot")
  rug(used_car_data$kmDriven,side=2)
  
  boxplot(used_car_data$Transmission, col = rainbow(6), ylab = "Transmission Boxplot")
  rug(used_car_data$Transmission,side=2)
  
  boxplot(used_car_data$FuelType, col = rainbow(6), ylab = "FuelType Boxplot")
  rug(used_car_data$FuelType,side=2)
  
  boxplot(used_car_data$AskPrice, col = rainbow(6), ylab = "AskPrice Boxplot")
  rug(used_car_data$AskPrice,side=2)
  
  boxplot(used_car_data, col = rainbow(6), ylab = "used_car_data Boxplot")

  
  # using Naive Base
  
  install.packages("e1071")
  library(e1071)
  
  dtrain <- used_car_data[1:8000, ]  # Training data (rows 1 to 7180)
  dtest <- used_car_data[8001:9582, ]  # Testing data (rows 7181 to 9582)
  
  # Separate features (x) and target variable (y) for training and testing
  x_train <- dtrain[, -7]  # Features for training (all columns except the 7th)
  y_train <- dtrain[, 7]   # Target variable for training (7th column)
  
  x_test <- dtest[, -7]    # Features for testing
  y_test <- dtest[, 7]     # Target variable for testing
  
  # Train the Naive Bayes model using x and y
  library(e1071)  # Ensure the e1071 library is loaded for naiveBayes
  ClassCars <- naiveBayes(x_train, y_train)
  
  # Predict the target variable on the test data
  PricePrediction <- predict(ClassCars, x_test)
  
  # Calculate accuracy
  correct_predictions <- sum(PricePrediction == y_test)  # Count correct predictions
  accuracy <- correct_predictions / length(y_test)  # Calculate accuracy
  
  # Print results
  print("Predicted Prices:")
  print(PricePrediction)
  print(paste("Accuracy:", accuracy*100))
  
  
  
  #########################################33
  
  
  install.packages("class")
  install.packages("caret")
  
  # Installing Packages 
  install.packages("e1071") 
  install.packages("caTools") 

  # Loading package 
  library(e1071) 
  library(caTools) 
  library(class)

  split <- sample.split(used_car_data, SplitRatio = 0.7) 
  train_cl <- subset(used_car_data, split == "TRUE") 
  test_cl <- subset(used_car_data, split == "FALSE") 
  print(test_cl)
  # Feature Scaling 
  train_scale <- train_cl[, 1:6]
  test_scale <- test_cl[, 1:6]
  
  print(train_scale)
  
  
  knn_pred <- knn(train = train_scale, test = test_scale, cl = train_cl$AskPrice, k = 19)
  correct_predictions_knn <- sum(knn_pred == y_test)
  print(correct_predictions_knn)
  print(knn_pred)
  accuracy_knn <- correct_predictions_knn / length(y_test)
  print(accuracy_knn*100)
  
  
  
