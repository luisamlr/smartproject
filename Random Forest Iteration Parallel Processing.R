#This file is for running the Random Forest iteration using all the cores of your PC.
# I think is not compatible with m1 and m2 mac books.

install.packages(c("foreach", "doParallel"))
library(foreach)
library(doParallel)
num_cores <- detectCores()
registerDoParallel(cores = num_cores)

# Create an empty dataframe to store the results
cv_results <- data.frame(n_features = integer(), mean_rmse = double())

# Loop through different numbers of features using foreach
cv_results <- foreach(n_features = 1:length(importance), .combine = rbind) %dopar% {
  library(ranger)
  
  # Identify the names of the top features
  top_features <- names(importance)[order(importance, decreasing = TRUE)][1:n_features]
  
  # Create a dataset with only the selected features
  train_data_selected <- train_data[, c(top_features, "Rating")]
  
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
  
  # Store the results as a data frame
  result <- data.frame(n_features = n_features, mean_rmse = mean_rmse)
  
  print(paste0("Iteration number ", n_features, "/", length(importance)))
  return(result)
}

# Stop the parallel backend
stopImplicitCluster()
