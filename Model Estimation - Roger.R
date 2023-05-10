# Bayesian Model
df_model <- read_excel("reshaped_poi_locations.xlsx")

# Define the number of bootstrap samples
n_boot <- 1000

# Create an empty vector to store the RMSE values
rmse_values <- numeric(n_boot)

# Perform bootstrapping
for (i in 1:n_boot) {
  # Resample the test data with replacement
  resampled_data <- test_data[sample(nrow(test_data), replace = TRUE), ]
  
  # Make predictions on the resampled data
  resampled_predictions <- predict(rf_model, data = resampled_data)$predictionsRs
  
  # Calculate the RMSE on the resampled predictions
  resampled_rmse <- sqrt(mean((resampled_predictions - resampled_data$Rating)^2))
  
  # Store the RMSE value
  rmse_values[i] <- resampled_rmse
}

# Compute the lower and upper bounds of the 95% confidence interval
lower_bound <- quantile(rmse_values, 0.025)
upper_bound <- quantile(rmse_values, 0.975)

# Print the confidence interval
cat("95% Confidence Interval for RMSE:", lower_bound, "-", upper_bound)

cor(predictions,actual_values)
# Load required library for plotting
library(ggplot2)

# Create a data frame with predictions and actual_values
data <- data.frame(predictions, actual_values)

# Plot the correlation using a scatter plot
ggplot(data, aes(x = predictions, y = actual_values)) +
  geom_point() +
  labs(x = "Predictions", y = "Actual Values") +
  ggtitle("Correlation between Predictions and Actual Values") +
  xlim(0, 5)

library(Rserve)
Rserve(args="--save")


library(randomForest)
library(caret)
library(xgboost)
library(rstanarm)
library(nnet)
library(readxl)
library(e1071)
library(gbm)

df_model <- read_excel("reshaped_poi_locations.xlsx")

# Set up 10-fold cross-validation
num_folds <- 10
folds <- createFolds(df_model$Rating, k = num_folds, list = TRUE, returnTrain = TRUE)

# Initialize vectors to store RMSE values for each model
rf_rmse_train <- numeric(num_folds)
rf_rmse_test <- numeric(num_folds)
xgb_rmse_train <- numeric(num_folds)
xgb_rmse_test <- numeric(num_folds)
svm_rmse_train <- numeric(num_folds)
svm_rmse_test <- numeric(num_folds)
gbm_rmse_train <- numeric(num_folds)
gbm_rmse_test <- numeric(num_folds)

# Perform cross-validation
for (i in 1:num_folds) {
  # Get training and testing data for the current fold
  train_data <- df_model[folds[[i]], ]
  test_data <- df_model[-folds[[i]], ]
  
  # Random Forest
  rf_model <- randomForest(formula = Rating ~ ., data = train_data)
  rf_pred_train <- predict(rf_model, newdata = train_data)
  rf_rmse_train[i] <- sqrt(mean((rf_pred_train - train_data$Rating)^2))
  
  rf_pred_test <- predict(rf_model, newdata = test_data)
  rf_rmse_test[i] <- sqrt(mean((rf_pred_test - test_data$Rating)^2))
  
  # Assuming "Rating" is the column name you want to find the number of
  column_name <- "Rating"
  column_number <- which(names(df_model) == column_name)
  
  # XGBoost
  xgb_model <- xgboost(data = as.matrix(train_data[, -column_number]), label = train_data$Rating, nrounds = 100)
  xgb_pred_train <- predict(xgb_model, newdata = as.matrix(train_data[, -column_number]))
  xgb_rmse_train[i] <- sqrt(mean((xgb_pred_train - train_data$Rating)^2))
  
  xgb_pred_test <- predict(xgb_model, newdata = as.matrix(test_data[, -column_number]))
  xgb_rmse_test[i] <- sqrt(mean((xgb_pred_test - test_data$Rating)^2))
  
  # SVM
  svm_model <- svm(Rating ~ ., data = train_data, kernel = "radial")
  svm_pred_train <- predict(svm_model, newdata = train_data)
  svm_rmse_train[i] <- sqrt(mean((svm_pred_train - train_data$Rating)^2))
  
  svm_pred_test <- predict(svm_model, newdata = test_data)
  svm_rmse_test[i] <- sqrt(mean((svm_pred_test - test_data$Rating)^2))
  
  # GBM
  gbm_model <- gbm(Rating ~ ., data = train_data, n.trees = 100, interaction.depth = 3, shrinkage = 0.1)
  gbm_pred_train <- predict(gbm_model, newdata = train_data, n.trees = 100)
  gbm_rmse_train[i] <- sqrt(mean((gbm_pred_train - train_data$Rating)^2))
  
  gbm_pred_test <- predict(gbm_model, newdata = test_data, n.trees = 100)
  gbm_rmse_test[i] <- sqrt(mean((gbm_pred_test - test_data$Rating)^2))
}

# Compute the average RMSE across all folds
avg_rf_rmse_train <- mean(rf_rmse_train)
avg_rf_rmse_test <- mean(rf_rmse_test)
avg_xgb_rmse_train <- mean(xgb_rmse_train)
avg_xgb_rmse_test <- mean(xgb_rmse_test)
avg_svm_rmse_train <- mean(svm_rmse_train)
avg_svm_rmse_test <- mean(svm_rmse_test)
avg_gbm_rmse_train <- mean(gbm_rmse_train)
avg_gbm_rmse_test <- mean(gbm_rmse_test)

# Print average RMSE for each model
print(paste("Random Forest Train RMSE:", avg_rf_rmse_train))
print(paste("Random Forest Test RMSE:", avg_rf_rmse_test))
print(paste("XGBoost Train RMSE:", avg_xgb_rmse_train))
print(paste("XGBoost Test RMSE:", avg_xgb_rmse_test))
print(paste("SVM Train RMSE:", avg_svm_rmse_train))
print(paste("SVM Test RMSE:", avg_svm_rmse_test))
print(paste("GBM Train RMSE:", avg_gbm_rmse_train))
print(paste("GBM Test RMSE:", avg_gbm_rmse_test))

