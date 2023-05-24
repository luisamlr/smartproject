set.seed(123)  # For reproducibility

require(xgboost)
require(caret)
library(openxlsx)
library(ranger)

reshaped_poi_locations <- read.xlsx("reshaped_poi_locations.xlsx")

df <- reshaped_poi_locations
splitIndex <- createDataPartition(df$Rating, p = .8, 
                                  list = FALSE, 
                                  times = 1)

trainData <- df[ splitIndex,]
testData  <- df[-splitIndex,]

# Build the model using the training data
rf_model <- ranger(Rating ~ ., data = trainData, importance = "permutation")

# Extract the variable importance
importance <- rf_model$variable.importance

# Create a 5-fold cross-validation scheme
cv_scheme <- createFolds(trainData$Rating, k = 5)

# Initialize variables to store the results
cv_results <- data.frame(n_features = integer(), mean_rmse = numeric())

# Random Forest model calculation

# Want to find to iterate all variables (takes around 40 min) or calculate the full model (ITERATE/FULL)
model_cal <- "ITERATE"

if (model_cal == "ITERATE") {
  
  # Loop through different numbers of features
  for (n_features in 1:length(importance)) {
    
    # Identify the names of the top features
    top_features <- names(importance)[order(importance, decreasing = TRUE)][1:n_features]
    
    # Create a dataset with only the selected features
    train_data_selected <- trainData[, c(top_features, "Rating")]
    
    # Initialize a variable to store the RMSE values for each fold
    fold_rmse <- numeric()
    
    # Perform cross-validation for the current feature subset
    for (i in 1:length(cv_scheme)) {
      train_fold <- train_data_selected[-cv_scheme[[i]], ]
      test_fold <- train_data_selected[cv_scheme[[i]], ]
      
      # Train the model with the current feature subset
      rf_model_selected <- ranger(Rating ~ ., data = train_fold)
      
      # Make predictions for the test fold
      predictions_selected <- predict(rf_model_selected, data = test_fold)$predictions
      
      # Calculate the RMSE for the test fold
      fold_rmse[i] <- sqrt(mean((test_fold$Rating - predictions_selected)^2))
    }
    
    # Calculate the mean RMSE across all folds
    mean_rmse <- mean(fold_rmse)
    
    # Store the results
    cv_results <- rbind(cv_results, data.frame(n_features = n_features, mean_rmse = mean_rmse))
    
    print(paste0("Iteration number ",n_features,"/",length(importance)))
  }
  
  # Find the best number of features based on the lowest mean RMSE
  best_n_features <- cv_results[which.min(cv_results$mean_rmse), "n_features"]
  cat("Best number of features:", best_n_features)
  
  # Select the best features names.
  best_top_features <- names(importance)[order(importance, decreasing = TRUE)][1:best_n_features]
  best_top_features
  
  # Select only the variables from the best top features.
  train_data_selected <- trainData[, c(best_top_features, "Rating")]
  
} else {
  # Initialize a variable to store the RMSE values for each fold
  fold_rmse_full <- numeric()
  
  # Perform cross-validation for the full model
  for (i in 1:length(cv_scheme)) {
    train_fold_full <- trainData[-cv_scheme[[i]], ]
    test_fold_full <- trainData[cv_scheme[[i]], ]
    
    # Train the model with all features
    rf_model_full <- ranger(Rating ~ ., data = train_fold_full)
    
    # Make predictions for the test fold
    predictions_full <- predict(rf_model_full, data = test_fold_full)$predictions
    
    # Calculate the RMSE for the test fold
    fold_rmse_full[i] <- sqrt(mean((test_fold_full$Rating - predictions_full)^2))
  }
  
  # Calculate the mean RMSE across all folds for the full model
  mean_rmse_full <- mean(fold_rmse_full)
  cat("Mean RMSE for the full model:", mean_rmse_full)
  
  # Use all features for the final model
  best_top_features <- colnames(trainData[, -ncol(trainData)]) # Exclude the "Rating" column
  train_data_selected <- trainData
}


# 
final_rf_model <- ranger(Rating ~ ., data = train_data_selected)

test_data_selected <- test_data[, best_top_features]

predictions <- predict(final_rf_model, data = test_data_selected)$predictions







# SVM

final_svm_model <- svm(Rating ~ ., data = train_data)
test_data_selected_svm <- test_data[, best_top_features_svm]
predictions_svm <- predict(final_svm_model, newdata = test_data_selected_svm)