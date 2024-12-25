# installing the needed libraries 



install.packages(c("dplyr", "lubridate", "tidyr"))
install.packages(c("rpart", "rpart.plot"))
install.packages("e1071")

library(rpart)
library(rpart.plot)
library(dplyr)
library(lubridate)
library(tidyr)
install.packages('caret', dependencies = TRUE)
install.packages("xgboost")
install.packages("caret")
library(caret)
install.packages("randomForest")
library(randomForest)
library(e1071)



# then read the data set 
used_car_data <- read.csv('used_car_dataset.csv')
DF<-data.frame()
View(used_car_data)
head(used_car_data)
dim(used_car_data)
names(used_car_data)

#----------------------------------------------------------------------------------- starting preprocessing --------------------------------------------------

# Drop multiple columns together by specifying their indices
#-----------------------------------------------------------
used_car_data <- used_car_data[, -c(3, 7, 9,10)]



# check if there is any missing values we delete it and it will not affect in the dataset size 
#---------------------------------------------------------------------------------------------
#check the number of missing values in each column before deleting 
missing_values_before <- colSums(is.na(used_car_data))
print(missing_values_before)
used_car_data_cleaned <- na.omit(used_car_data)
# Check the number of missing values in each column after deletion
missing_values_after <- colSums(is.na(used_car_data_cleaned))
print(missing_values_after)


#check if there is any duplicated data and handle it
#---------------------------------------------------


duplicated_rows <- used_car_data[duplicated(used_car_data), ]
nrow(duplicated_rows)

# there is 748 dupliactes rows so we will delete them by this 
used_car_data <- unique(used_car_data)
dim(used_car_data)

#check again if there is any duplicated data after deleting them all 
duplicated_rows <- used_car_data[duplicated(used_car_data), ]
nrow(duplicated_rows)


#convert characters
#--------------------
used_car_data$kmDriven <- as.numeric(gsub("[ km,]", "", used_car_data$kmDriven))


used_car_data$AskPrice <- as.numeric(gsub('[₹,]', '', used_car_data$AskPrice))

used_car_data$Brand <- as.factor(used_car_data$Brand)
used_car_data$Brand <- as.numeric(used_car_data$Brand)

used_car_data$model <- as.factor(used_car_data$model)
used_car_data$model <- as.numeric(used_car_data$model)

#used_car_data$kmDriven <- as.factor(used_car_data$kmDriven)
#used_car_data$kmDriven <- as.numeric(used_car_data$kmDriven)

used_car_data$Transmission <- as.factor(used_car_data$Transmission)
used_car_data$Transmission <- as.numeric(used_car_data$Transmission)

used_car_data$FuelType <- as.factor(used_car_data$FuelType)
used_car_data$FuelType <- as.numeric(used_car_data$FuelType)
#used_car_data$AskPrice <- as.factor(used_car_data$AskPrice)
#used_car_data$AskPrice <- as.numeric(used_car_data$AskPrice)


a<-100
b<-1000

#used_car_data$Brand <- a+(used_car_data$Brand - min(used_car_data$Brand, na.rm = TRUE))*(b-a) / (max(used_car_data$Brand, na.rm = TRUE) - min(used_car_data$Brand, na.rm = TRUE))
#used_car_data$model <- a+(used_car_data$model - min(used_car_data$model, na.rm = TRUE))*(b-a) / (max(used_car_data$model, na.rm = TRUE) - min(used_car_data$model, na.rm = TRUE))

#km
#used_car_data$kmDriven <- round(a+(used_car_data$kmDriven - min(used_car_data$kmDriven, na.rm = TRUE))*(b-a) / (max(used_car_data$kmDriven, na.rm = TRUE) - min(used_car_data$kmDriven, na.rm = TRUE)),0)
#used_car_data$AskPrice <- round(a+(used_car_data$AskPrice - min(used_car_data$AskPrice, na.rm = TRUE))*(b-a) / (max(used_car_data$AskPrice, na.rm = TRUE) - min(used_car_data$AskPrice, na.rm = TRUE)),0)

#check if any column has any negative value 
#-------------------------------------------


# Check for negative values in specific columns
negative_values <- sapply(used_car_data[, c("Age", "kmDriven", "AskPrice")], function(x) sum(x < 0, na.rm = TRUE))

# Print the number of negative values in each of the specified columns
print(negative_values)
# number of negative values in each column is 0 so there is no any negative values in the dataset 




#KmDriven_counts <- table(used_car_data$kmDriven)

#sorted_kmDriven_counts <- sort(KmDriven_counts, decreasing = TRUE)
#most_frequent_kmDriven <- names(sorted_kmDriven_counts)[1]

#print(most_frequent_kmDriven)


#used_car_data$PostedDate <- dmy(used_car_data$PostedDate)



#used_car_data$Age[used_car_data$Age<0]<-used_car_data$Age[used_car_data$Age<0]*(-1)   there is no any negative values in this column 




#used_car_data$KmDriven <- as.character(used_car_data$KmDriven)

#.str.replace(' km', '').str.replace(',', '').astype(float)






str(used_car_data)
par(mfrow=c(2,3))


#__founding outliers in Age
dev.new()
boxplot(used_car_data$Age, col = rainbow(6), ylab = "Age Boxplot")
rug(used_car_data$Age,side=2)

age_out_rm <- boxplot.stats(used_car_data$Age)$out
out_in <- which(used_car_data$Age %in% c(age_out_rm))

used_car_data<-used_car_data[-out_in,] # remove outliers
dim(used_car_data)

dev.new()
boxplot(used_car_data[-out_in,]$Age, col = rainbow(6), ylab = "Age Boxplot")
rug(used_car_data$Age,side=2)



#------------------------------------------------------------------------------------VISULAIZATIONS -------------------------------------------------------------

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





#____________________________

boxplot(used_car_data$kmDriven, col = rainbow(6), ylab = "kmDriven Boxplot")
rug(used_car_data$kmDriven,side=2)


boxplot(used_car_data$Transmission, col = rainbow(6), ylab = "Transmission Boxplot")
rug(used_car_data$Transmission,side=2)

boxplot(used_car_data$FuelType, col = rainbow(6), ylab = "FuelType Boxplot")
rug(used_car_data$FuelType,side=2)

boxplot(used_car_data$AskPrice, col = rainbow(6), ylab = "AskPrice Boxplot")
rug(used_car_data$AskPrice,side=2)

boxplot(used_car_data$Age, col = rainbow(6), ylab = "Age Boxplot")
rug(used_car_data$AskPrice,side=2)

boxplot(used_car_data, col = rainbow(6), ylab = "used_car_data Boxplot")





#spliting the dataset into 2 sets 80% into training dataset and the 20% will be the testing dataset
set.seed(123) #34an admn en lt2sem ykon sabt kol mra nsh8l feha el code 34an nfs el result tzhr kol mra
trainIndex <- createDataPartition(used_car_data$AskPrice, p = 0.8, list = FALSE)

train_data <- used_car_data[trainIndex, ]
test_data <- used_car_data[-trainIndex, ]




##_______________MODELS________________#

# 1. Linear Regression


dim(trainIndex)
dim(train_data)
dim(test_data)

# training model 
regression_model <- lm(train_data$AskPrice ~. , data = train_data)

predict_on_train <- predict(regression_model,train_data)


# Add regression line

dev.new()
plot( predict_on_train, train_data$AskPrice, main = "Scatterplot with Regression Line",
     xlab = predict_on_train, ylab = train_data$AskPrice, pch = 16, col = "blue")
# Add regression line
abline(0,1)

#Install Package
install.packages("hydroGOF")

#Load Library
library(hydroGOF)

#Calculate RMSE 
RMSE=rmse(predict_on_train,train_data$AskPrice)

print(RMSE)

str(regression_model)
dim(train_data)


#calculate rmse for test data

pred_reg_test <- predict(regression_model, test_data)

rmse_train=rmse(pred_reg_test,test_data$AskPrice)


r2_lm <- cor(pred_reg_test,test_data$AskPrice)^2

print(r2_lm)

-----------------------------------------------------------------------------------------------------------------------------------------------------------
  #  Trying SVm
  
  svm_model <- svm(train_data, train_data$AskPrice, type = "eps-regression", kernel = "radial")

# Predict on the test set
predictions_svm <- predict(svm_model, test_data, type= "response")

print(svm_model)
# Calculate accuracy metrics
mae <- mean(abs(predictions_svm - test_data$AskPrice))  # Mean Absolute Error
mse <- mean((predictions_svm - test_data$AskPrice)^2)  # Mean Squared Error
rmse <- sqrt(mse)

r2 <- cor(predictions_svm,test_data$AskPrice)^2
r2 <- 1 - (sum((test_data$AskPrice - predictions_svm)^2) / sum((test_data$AskPrice - mean(predictions_svm))^2))
           # Print metrics
cat("Mean Absolute Error (MAE):", format(mae, scientific = FALSE), "\
")
cat("Root Mean Squared Error (RMSE):", format(rmse, scientific = FALSE), "\
")
cat("R-squared (R2):", round(r2, 4))
-----------------------------------------------------------------------------------------------------------------------------------------------------------
  # 2. RANDOM FOREST MODEL HAFSA ELSHERERAAAAAA
  
  
  rf_model_improved <- randomForest(
    AskPrice ~ Brand + model  + Age  + Transmission +
      FuelType ,
    data = train_data,
    ntree = 1000,
    mtry = 5,
    nodesize = 10,
    importance = TRUE
  )

rf_pred_improved <- predict(rf_model_improved, test_data)
rf_rmse_improved <- sqrt(mean((rf_pred_improved - test_data$AskPrice)^2))
rf_r2_improved <- cor(rf_pred_improved, test_data$AskPrice)^2
print(paste("Improved Random Forest R-squared:", round(rf_r2_improved, 3)))
print(paste("Improved Random Forest RMSE:", format(rf_rmse_improved, scientific = FALSE)))



---------------------------------------------------------------------------------------------------------------------------------------------
  # 3. DECISION TREE FOR HABIBA ELTAYPA 
  
  # the acc without feature engineering is 63%
  # the acc without feature engineering but with this code is 72 % 
  


# Create interaction terms and additional features
dt_model_enhanced <- rpart(
  AskPrice ~ Brand + model + Age + kmDriven + Transmission +
    I(Age^2) + I(kmDriven^2) + # Add polynomial terms
    I(Age * kmDriven) + # Add interaction term
    I(log(kmDriven + 1)), # Add log transformation
  data = train_data,
  method = "anova",
  control = rpart.control(
    minsplit = 2,
    minbucket = 1,
    maxdepth = 15,
    cp = 0.0001  # Reduced complexity parameter for more detailed tree
  )
)

# Cross-validation to find optimal complexity parameter
printcp(dt_model_enhanced)
opt_cp <- dt_model_enhanced$cptable[which.min(dt_model_enhanced$cptable[,"xerror"]), "CP"]
dt_model_pruned <- prune(dt_model_enhanced, cp = opt_cp)

# Make predictions and calculate metrics
dt_pred <- predict(dt_model_pruned, test_data)
dt_rmse <- sqrt(mean((dt_pred - test_data$AskPrice)^2))
dt_r2 <- cor(dt_pred, test_data$AskPrice)^2

print(paste("Enhanced Decision Tree RMSE:", format(dt_rmse, scientific = FALSE)))
print(paste("Enhanced Decision Tree R-squared:", round(dt_r2, 3)))

---------------------------------------------------------------------------------------------------------------------------------------------------
  
  # 4. XGBOOST MODEL 
library(stringr)  #may be removed
library(xgboost)


# Prepare matrices for xgboost
train_matrix <- as.matrix(train_data[, !colnames(train_data) %in% "AskPrice"])
test_matrix <- as.matrix(test_data[, !colnames(test_data) %in% "AskPrice"])

# Train XGBoost model
xgb_model <- xgboost(data = train_matrix, 
                     label = train_data$AskPrice,
                     nrounds = 100,
                     objective = "reg:squarederror",
                     verbose = 0)

# Make predictions
predictions <- predict(xgb_model, test_matrix)

# Calculate RMSE and R-squared
rmse <- sqrt(mean((test_data$AskPrice - predictions)^2))
r2 <- cor(test_data$AskPrice, predictions)^2

# Print metrics
print(paste("RMSE:", format(rmse, scientific = FALSE)))
print(paste("R-squared:", round(r2, 4)))

------------------------------------------------------------------------------------------------------------------------------------------------------------
  