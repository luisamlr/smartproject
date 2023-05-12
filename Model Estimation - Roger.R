# Load required library for plotting
library(ggplot2)
library(readxl)
library(randomForest)
library(caret)
library(xgboost)
library(rstanarm)
library(nnet)
library(readxl)
library(e1071)
library(gbm)

set.seed(123)
# Import dataset
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


##### Model #### 
library(ranger)

# Import dataset
df_model <- read_excel("reshaped_poi_locations.xlsx")
potential_CS <- read_excel("potential_CS.xlsx")

add_missing_columns <- function(df1, df2) {
  missing_cols_df1 <- setdiff(names(df2), names(df1))
  missing_cols_df2 <- setdiff(names(df1), names(df2))
  
  # Add missing columns to df1 and fill with zeros
  for (col in missing_cols_df1) {
    df1[[col]] <- 0
  }
  
  # Add missing columns to df2 and fill with zeros
  for (col in missing_cols_df2) {
    df2[[col]] <- 0
  }
  
  return(list(df1 = df1, df2 = df2))
}

# Example usage with potential_CS and df_model
result <- add_missing_columns(potential_CS, df_model)
potential_CS <- result$df1
df_model<- result$df2

# Output the modified data frames
print(potential_CS)
print(df_model)

rf_model <- randomForest(formula = Rating ~ ., data = df_model)
lm_model <-lm(formula = Rating ~ ., data = df_model)
gbm_model <- gbm(Rating ~ ., data = df_model, n.trees = 100, interaction.depth = 3, shrinkage = 0.1)

# Improve the model:
library(gbm)
library(caret)

# Define the parameter grid
parameter_grid <- expand.grid(n.trees = c(50, 100, 150),
                              interaction.depth = c(2, 3, 4),
                              shrinkage = c(0.1, 0.2, 0.3))

# Initialize variables for best parameters and RMSE
best_params <- NULL
best_rmse <- Inf

# Perform grid search
for (i in 1:nrow(parameter_grid)) {
  # Fit the model with current parameter combination
  gbm_model <- gbm(Rating ~ ., data = df_model,
                   n.trees = parameter_grid$n.trees[i],
                   interaction.depth = parameter_grid$interaction.depth[i],
                   shrinkage = parameter_grid$shrinkage[i])
  
  # Generate predictions
  gbm_pred <- predict(gbm_model, newdata = df_model, n.trees = parameter_grid$n.trees[i])
  
  # Calculate RMSE
  rmse <- sqrt(mean((df_model$Rating - gbm_pred)^2))
  
  # Check if current parameter combination is the best
  if (rmse < best_rmse) {
    best_params <- parameter_grid[i, ]
    best_rmse <- rmse
  }
}

# Print the best parameter combination and RMSE
cat("Best Parameters:\n")
print(best_params)
cat("RMSE:", best_rmse)
library(gbm)
library(caret)

# Define the parameter grid with corrected column names
parameter_grid <- expand.grid(n.trees = c(50, 75, 100, 125, 150, 175, 200, 225, 250, 275),
                              interaction.depth = c(2, 3, 4, 5, 6, 7, 8),
                              shrinkage = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
                              n.minobsinnode = 10)

# Set up cross-validation
ctrl <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# Perform grid search and cross-validation
gbm_model <- train(Rating ~ ., data = df_model,
                   method = "gbm",
                   trControl = ctrl,
                   tuneGrid = parameter_grid)

# Get the best model
best_model <- gbm_model$finalModel

# Generate predictions on the training set
gbm_pred <- predict(best_model, newdata = df_model, n.trees = best_model$n.trees)

# Calculate RMSE on the training set
rmse <- sqrt(mean((df_model$Rating - gbm_pred)^2))

# Print the best parameters and RMSE
cat("Best Parameters:\n")
print(gbm_model$bestTune)
cat("RMSE on training set:", rmse)






residuals <- df_model$Rating - predict(gbm_model, newdata = df_model, n.trees = 150 , interaction.depth = 4, shrinkage = 0.3)
cor(df_model$Rating, predict(gbm_model, newdata = df_model, n.trees = 100))

# Calculate the correlation
correlation <- cor(df_model$Rating, predict(gbm_model, newdata = df_model, n.trees = 100))

# Plot the correlation
plot(df_model$Rating, predict(gbm_model, newdata = df_model, n.trees = 100),
     xlab = "Actual Ratings", ylab = "Predicted Ratings",
     main = paste("Correlation:", round(correlation, 2)))
library(ggplot2)

# Create a data frame with the actual and predicted ratings
df <- data.frame(Actual = df_model$Rating,
                 Predicted = predict(gbm_model, newdata = df_model, n.trees = 100))

# Calculate the correlation
correlation <- cor(df$Actual, df$Predicted)

# Create the scatter plot
ggplot(df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  xlab("Actual Ratings") +
  ylab("Predicted Ratings") +
  ggtitle(paste("Correlation:", round(correlation, 2)))+
  ylim(0,5)


standard_error <- sd(residuals)
# Generate predictions
gbm_pred <- predict(gbm_model, newdata = potential_CS, n.trees = 100)

critical_value <- 1.96
lower_bound <- gbm_pred - critical_value * standard_error
upper_bound <- gbm_pred + critical_value * standard_error
confidence_interval <- c(lower_bound, upper_bound)


final<-cbind(potential_CS, Prediction = gbm_pred, lower_bound, upper_bound)

library(writexl)
# Export data frame to an Excel file
write_xlsx(final, "potential_CS_final.xlsx") 
