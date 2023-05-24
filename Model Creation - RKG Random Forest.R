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

# Define the parameter grid
num.trees <- c(100, 200, 300)
min.node.size <- c(1, 5, 10)
sample.fraction <- c(0.5, 0.75, 1)

# Create 5-fold cross-validation indices
folds <- createFolds(trainData$Rating, k = 5)

# Initialize a list to store the models and their RMSEs
models <- list()
model_rmse <- numeric(length(num.trees) * length(min.node.size) * length(sample.fraction) * length(folds))

# Counter to keep track of the model
counter <- 1

# Iterate over the parameter grid
for (trees in num.trees) {
  for (node.size in min.node.size) {
    for (fraction in sample.fraction) {
      
      # Perform 5-fold cross-validation
      for(i in seq_along(folds)){
        # Segregate the data into training and validation sets
        validationData <- trainData[folds[[i]], ]
        crossTrainData <- trainData[-folds[[i]], ]
        
        # Train the model with all features
        rf_model <- ranger(Rating ~ ., data = crossTrainData, num.trees = trees, 
                           min.node.size = node.size, sample.fraction = fraction)
        
        # Make predictions for the validation set
        predictions <- predict(rf_model, data = validationData)$predictions
        
        # Calculate the RMSE for the validation set
        rmse <- sqrt(mean((validationData$Rating - predictions)^2))
        
        # Store the model and its RMSE
        models[[counter]] <- rf_model
        model_rmse[counter] <- rmse
        
        # Update the counter
        counter <- counter + 1
      }
    }
  }
}

# Find the model with the lowest average RMSE across the 5 folds
best_model_index <- which.min(model_rmse)
best_model <- models[[best_model_index]]

print(paste("Best model RMSE: ", model_rmse[best_model_index]))

# Make predictions on the test data using the best model
test_predictions <- predict(best_model, data = testData)$predictions

# Calculate the RMSE for the test data
test_rmse <- sqrt(mean((testData$Rating - test_predictions)^2))

# Print the test RMSE
print(paste("Test RMSE: ", test_rmse))

# Add the predictions as a new column to the test data
testData$Predictions <- predict(best_model, data = testData)$predictions


