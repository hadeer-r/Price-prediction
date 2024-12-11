setwd("C:/Users/Esraa/Downloads/statistical inference/Project Statistical")
used_car_data <- read.csv("used_car_dataset.csv", na.strings="")
DF<-data.frame()
View(used_car_data)
dim(used_car_data)
names(used_car_data)

colSums(is.na(used_car_data))

KmDriven_counts <- table(used_car_data$kmDriven)
KmDriven_counts <- table(used_car_data$kmDriven)
sorted_kmDriven_counts <- sort(KmDriven_counts, decreasing = TRUE)
most_frequent_kmDriven <- names(sorted_kmDriven_counts)[1]
print(most_frequent_kmDriven)
used_car_data$kmDriven[is.na(used_car_data$kmDriven)]<-most_frequent_kmDriven

colSums(is.na(used_car_data))


 
  used_car_data <- used_car_data[, -3]
 
  used_car_data$Age[used_car_data$Age<0]<-used_car_data$Age[used_car_data$Age<0]*(-1)
  
  used_car_data <- used_car_data[, -6]
  
  used_car_data <- used_car_data[, -8]
  
 # used_car_data$KmDriven <- as.character(used_car_data$KmDriven)
  

  #used_car_data$KmDriven <- sub("km", "", used_car_data$KmDriven)
  
  
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
  
  