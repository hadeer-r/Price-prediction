# installing the needed libraries 


# Function to remove outliers
remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- IQR(x, na.rm = TRUE)
  x[x >= (Q1 - 1.5 * IQR_value) & x <= (Q3 + 1.5 * IQR_value)]
}



#_________Instailling needed packages_____________
install.packages(c("dplyr", "lubridate", "tidyr"))
install.packages(c("rpart", "rpart.plot"))
install.packages("e1071")
install.packages('caret', dependencies = TRUE)
install.packages("xgboost")
install.packages("randomForest")

library(rpart)
library(rpart.plot)
library(dplyr)
library(lubridate)
library(tidyr)
library(caret)
library(randomForest)
library(e1071)

#---------------------------------------------------------------------------------------------------------------------------------------------

#______________Load Data__________________
used_car_data <- read.csv("used_car_dataset.csv", na.strings="")

View(used_car_data)
dim(used_car_data)
names(used_car_data)

# Remove Nulls
colSums(is.na(used_car_data))

used_car_data <- na.omit(used_car_data)

colSums(is.na(used_car_data))

# Drop unuseful columns 

used_car_data <- used_car_data[, -c(3, 7, 9,10)]


# repair negatives values in age column

used_car_data$Age[used_car_data$Age<0]<-used_car_data$Age[used_car_data$Age<0]*(-1)


# remove sign character from kmDriven and AskPrice

used_car_data$kmDriven <- as.numeric(gsub("[ km,]", "", used_car_data$kmDriven))
used_car_data$AskPrice <- as.numeric(gsub('[₹,]', '', used_car_data$AskPrice))


# convert text to numeric values by label encoding

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


#___________ Remove oultiers from Age AskPrice KmDriven


#kmDriven
dev.new()
boxplot(used_car_data$kmDriven, col = rainbow(6), ylab = "kmDriven Boxplot")
rug(used_car_data$kmDriven,side=2)

used_car_data <- used_car_data[!is.na(used_car_data$kmDriven) & used_car_data$kmDriven %in% remove_outliers(used_car_data$kmDriven), ]

dev.new()
boxplot(used_car_data$kmDriven, col = rainbow(6), ylab = "kmDriven Boxplot")
rug(used_car_data$kmDriven,side=2)


#Age
dev.new()
boxplot(used_car_data$Age, col = rainbow(6), ylab = "Age Boxplot")
rug(used_car_data$Age,side=2)

used_car_data <- used_car_data[!is.na(used_car_data$Age) & used_car_data$Age %in% remove_outliers(used_car_data$used_car_data), ]

dev.new()
boxplot(used_car_data[-out_in,]$Age, col = rainbow(6), ylab = "Age Boxplot")
rug(used_car_data$Age,side=2)

#AskPrice
dev.new()
boxplot(used_car_data$AskPrice, col = rainbow(6), ylab = "AskPrice Boxplot")
rug(used_car_data$AskPrice,side=2)

used_car_data <- used_car_data[!is.na(used_car_data$AskPrice) & used_car_data$AskPrice %in% remove_outliers(used_car_data$AskPrice), ]

dev.new()
boxplot(used_car_data$AskPrice, col = rainbow(6), ylab = "AskPrice Boxplot")
rug(used_car_data$AskPrice,side=2)


# Removing outliers in Age
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
#__________________________________________

# Removing outliers from kmDriven

dev.new()
boxplot(used_car_data$kmDriven, col = rainbow(6), ylab = "kmDriven Boxplot")
rug(used_car_data$kmDriven,side=2)

kmDriven_out_rm <- boxplot.stats(used_car_data$kmDriven)$out
kmDriven_out_in <- which(used_car_data$kmDriven %in% c(kmDriven_out_rm))

used_car_data<-used_car_data[-kmDriven_out_in,] # remove outliers
dim(used_car_data)

dev.new()
boxplot(used_car_data$kmDriven, col = rainbow(6), ylab = "kmDriven Boxplot")
rug(used_car_data$kmDriven,side=2)

#__________________________________________

# Removing outliers from AskPrice

dev.new()
boxplot(used_car_data$AskPrice, col = rainbow(6), ylab = "AskPrice Boxplot")
rug(used_car_data$AskPrice,side=2)

AskPrice_out_rm <- boxplot.stats(used_car_data$AskPrice)$out
AskPrice_out_in <- which(used_car_data$AskPrice %in% c(AskPrice_out_rm))

used_car_data<-used_car_data[-AskPrice_out_in,] # remove outliers
dim(used_car_data)

dev.new()
boxplot(used_car_data$AskPrice, col = rainbow(6), ylab = "AskPrice Boxplot")
rug(used_car_data$AskPrice,side=2)

#__________________________________________
# scalling by min/max method from 0 to 1

#used_car_data$Brand <- (used_car_data$Brand - min(used_car_data$Brand, na.rm = TRUE)) / (max(used_car_data$Brand, na.rm = TRUE) - min(used_car_data$Brand, na.rm = TRUE))
#used_car_data$model <- (used_car_data$model - min(used_car_data$model, na.rm = TRUE)) / (max(used_car_data$model, na.rm = TRUE) - min(used_car_data$model, na.rm = TRUE))
#used_car_data$Age <- (used_car_data$Age - min(used_car_data$Age, na.rm = TRUE)) / (max(used_car_data$Age, na.rm = TRUE) - min(used_car_data$Age, na.rm = TRUE))


#used_car_data$Transmission <- (used_car_data$Transmission - min(used_car_data$Transmission, na.rm = TRUE)) / (max(used_car_data$Transmission, na.rm = TRUE) - min(used_car_data$Transmission, na.rm = TRUE))
#used_car_data$FuelType <- (used_car_data$FuelType - min(used_car_data$FuelType, na.rm = TRUE)) / (max(used_car_data$FuelType, na.rm = TRUE) - min(used_car_data$FuelType, na.rm = TRUE))


#used_car_data$kmDriven <- (used_car_data$kmDriven - min(used_car_data$kmDriven, na.rm = TRUE))/ (max(used_car_data$kmDriven, na.rm = TRUE) - min(used_car_data$kmDriven, na.rm = TRUE))
#used_car_data$AskPrice <- (used_car_data$AskPrice - min(used_car_data$AskPrice, na.rm = TRUE))/ (max(used_car_data$AskPrice, na.rm = TRUE) - min(used_car_data$AskPrice, na.rm = TRUE))


#a<-1
#b<-1000
#used_car_data$kmDriven <- round(a+(used_car_data$kmDriven - min(used_car_data$kmDriven, na.rm = TRUE))*(b-a) / (max(used_car_data$kmDriven, na.rm = TRUE) - min(used_car_data$kmDriven, na.rm = TRUE)),0)
#used_car_data$AskPrice <- round(a+(used_car_data$AskPrice - min(used_car_data$AskPrice, na.rm = TRUE))*(b-a) / (max(used_car_data$AskPrice, na.rm = TRUE) - min(used_car_data$AskPrice, na.rm = TRUE)),0)


# checking structure
str(used_car_data)
par(mfrow=c(2,3))



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
dev.new()
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
dev.new()
boxplot(used_car_data, col = rainbow(6), ylab = "used_car_data Boxplot")





#______________________________Spliting Data to Train________________________________
set.seed(123) #34an admn en lt2sem ykon sabt kol mra nsh8l feha el code 34an nfs el result tzhr kol mra

trainIndex <- createDataPartition(used_car_data$AskPrice, p = 0.8, list = FALSE)

train_data <- used_car_data[trainIndex, ]
test_data <- used_car_data[-trainIndex, ]




##_______________MODELS________________#

# 1. Linear Regression


# training model 
linear_model_cars <- lm(AskPrice ~. , data = train_data)

str(linear_model_cars)

# calculate rmse for trained data
used_car_reg_rmse_train <- train_data %>%
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

pred_reg_test <- predict(linear_model_cars, newdata = test_data)
reg_rmse_test <- sqrt(mean((pred_reg_test - test_data$AskPrice)^2))

r_squared <- cor(test_data$AskPrice, pred_reg_test)^2
print(r_squared)
print(reg_rmse_test)



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
  