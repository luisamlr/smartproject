# Load required library for plotting
library(ggplot2)
library(ranger)
library(readxl)
library(randomForest)
library(caret)
library(xgboost)
library(rstanarm)
library(nnet)
library(readxl)
library(writexl)
library(e1071)
library(gbm)
set.seed(42)  # Set seed for reproducibility

##### Model  DATA preparation#### 

# Import dataset
df_model <- read_excel("reshaped_poi_locations.xlsx")
potential_CS <- read_excel("potential_CS.xlsx")

# Function to obtain the same amount of columns
add_missing_columns <- function(df1, df2) {
  missing_cols_df1 <- setdiff(names(df1), names(df2))  # Find columns in df1 that are missing in df2
  
  # Add missing columns to df2 and fill with zeros
  for (col in missing_cols_df1) {
    df2[[col]] <- 0
  }
  
  # Remove columns from df2 that are not present in df1
  missing_cols_df2 <- setdiff(names(df2), names(df1))  # Find columns in df2 that are missing in df1
  df2 <- df2[, !names(df2) %in% missing_cols_df2]  # Remove columns from df2
  
  return(list(df1 = df1, df2 = df2))
}
names(potential_CS)
names(df_model)
rot<-setdiff(names(df_model), names(potential_CS))
rot
# Tranform dataset
result <- add_missing_columns(df_model, potential_CS)
potential_CS <- result$df2
df_model <- result$df1

# Separet the dataset into training-testing and validation
splitIndex <- createDataPartition(df_model$Rating, p = .9, 
                                  list = FALSE, 
                                  times = 1)

trainData <- df_model[ splitIndex,]
testData  <- df_model[-splitIndex,] # Validation dataset


###### GBM - Model and estimation#####
# Define the parameter grid
parameter_grid <- expand.grid(n.trees = c(25, 50, 75, 100, 125, 150),
                              interaction.depth = c(2, 3, 4),
                              shrinkage = c(0.1, 0.2, 0.3),
                              n.minobsinnode = c(5,10))

# Define the control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Perform grid search with cross-validation using caret
gbm_model <- train(Rating ~ ., data = trainData, method = "gbm",
                   trControl = ctrl, tuneGrid = parameter_grid)

# Print the best parameter combination and RMSE
cat("Best Parameters:\n")
print(gbm_model$bestTune)
cat("RMSE:", min(gbm_model[["results"]][["RMSE"]]))

# Fit the model with current parameter combination
gbm_model <- gbm(Rating ~ ., data = trainData,
                 n.trees = 25,
                 interaction.depth = 4,
                 shrinkage = 0.1)

# Generate predictions for the Validation set (this has not been trained or tested yet)
gbm_pred <- predict(gbm_model, newdata = testData, n.trees = 25)
gmb_v_rmse <- sqrt(mean((testData$Rating - gbm_pred)^2))

# Create a data frame with the actual and predicted ratings
df <- data.frame(Actual = testData$Rating,
                 Predicted = gbm_pred)

# Calculate the correlation
correlation <- cor(df$Actual, df$Predicted)

# Create the scatter plot
ggplot(df, aes(x =Predicted , y = Actual)) +
  geom_point() +
  xlab("Predicted Ratings") +
  ylab("Actual Ratings") +
  ggtitle(paste("Correlation:", round(correlation, 2)))+
  xlim(0,5)

standard_error <- sd(df$Actual-df$Predicted)

# Predict for new charging stations
prediction_new<- predict(gbm_model, newdata = potential_CS, n.trees = 25)

critical_value <- 1.96
lower_bound <- prediction_new - critical_value * standard_error
upper_bound <- prediction_new + critical_value * standard_error
confidence_interval <- c(lower_bound, upper_bound)

final_GBM<-cbind(potential_CS, Prediction =prediction_new, lower_bound, upper_bound)

# Export data frame to an Excel file
# write_xlsx(final, "potential_CS_final.xlsx") 

###### RF - Model and estimation####

# Define the parameter grid
parameter_grid <- expand.grid(num.trees = c(50, 100, 150),
                              max.depth = c(2, 3, 4),
                              mtry = c(2,3,4))

# Initialize variables for best parameters and RMSE
best_params <- NULL
best_rmse <- Inf

# Perform grid search with k-fold cross-validation
k <- 5  # Number of folds
set.seed(42)  # Set seed for reproducibility

for (i in 1:nrow(parameter_grid)) {
  # Initialize variable for average RMSE across folds
  avg_rmse <- 0
  
  for (fold in 1:k) {
    # Create training and testing indices for the current fold
    indices <- createFolds(trainData$Rating, k = k, list = TRUE)
    train_indices <- unlist(indices[-fold])
    test_indices <- indices[[fold]]
    
    # Split data into training and testing sets for the current fold
    train_data <- trainData[train_indices, ]
    test_data <- trainData[test_indices, ]
    
    # Fit the model with current parameter combination
    ranger_model <- ranger(Rating ~ ., data = train_data, num.trees = parameter_grid$num.trees[i],
                           max.depth = parameter_grid$max.depth[i])
    
    # Generate predictions for the testing set
    ranger_pred <- predict(ranger_model, data = test_data)$predictions
    
    # Calculate RMSE for the current fold
    fold_rmse <- sqrt(mean((test_data$Rating - ranger_pred)^2))
    
    # Accumulate RMSE across folds
    avg_rmse <- avg_rmse + fold_rmse
  }
  
  # Calculate average RMSE across folds
  avg_rmse <- avg_rmse / k
  
  # Check if current parameter combination is the best
  if (avg_rmse < best_rmse) {
    best_params <- parameter_grid[i, ]
    best_rmse <- avg_rmse
  }
}

# Print the best parameter combination and RMSE
cat("Best Parameters:\n")
print(best_params)
cat("RMSE:", best_rmse)

# Fit the model with current parameter combination
ranger_model <- ranger(Rating ~ ., data = trainData,
                       num.trees = 150,
                       max.depth = 3)

# Generate predictions for the testing set
ranger_pred <- predict(ranger_model, data = testData)$predictions

# Create a data frame with the actual and predicted ratings
df <- data.frame(Actual = trainData$Rating,
                 Predicted = ranger_pred)

# Calculate the correlation
correlation <- cor(df$Actual, df$Predicted)

# Create the scatter plot
ggplot(df, aes(x =Predicted , y = Actual)) +
  geom_point() +
  xlab("Predicted Ratings") +
  ylab("Actual Ratings") +
  ggtitle(paste("Correlation:", round(correlation, 2)))+
  xlim(0,5)


standard_error <- sd(df$Actual-df$Predicted)

# Predict for new charging stations
prediction_new<- predict(ranger_model, newdata = potential_CS, n.trees = 50)

critical_value <- 1.96
lower_bound <- prediction_new - critical_value * standard_error
upper_bound <- prediction_new + critical_value * standard_error
confidence_interval <- c(lower_bound, upper_bound)

final_ranger<-cbind(potential_CS, Prediction =prediction_new, lower_bound, upper_bound)

# Export data frame to an Excel file
# write_xlsx(final, "potential_CS_final.xlsx") 

###### XGB - Model and estimation####

# Convert the predictors and targets into matrices
trainDataMatrix <- as.matrix(trainData[,-which(names(trainData) %in% "Rating")])
trainLabels <- as.vector(trainData$Rating)

# Convert the data into an xgb.DMatrix object
dtrain <- xgb.DMatrix(data = trainDataMatrix, label = trainLabels)


# Define the grid
hyper_grid <- expand.grid(
  eta = c(0.1, 0.2, 0.3, 0.5),
  max_depth = c(1, 2, 3, 4, 5),
  nrounds = c(50, 70, 100, 250, 500)
)

# Initialize a data frame to store results
results <- data.frame(
  eta = numeric(),
  max_depth = numeric(),
  nrounds = numeric(),
  RMSE = numeric()
)

# For each row in the grid, run a 5-fold cross-validation
for(i in 1:nrow(hyper_grid)) {
  params$eta = hyper_grid$eta[i]
  params$max_depth = hyper_grid$max_depth[i]
  
  cv_model <- xgb.cv(
    params = params, 
    data = dtrain, 
    nrounds = hyper_grid$nrounds[i],
    nfold = 5,
    showsd = T,
    stratified = T,
    print_every_n = 10,
    early_stopping_rounds = 10,
    maximize = F
  )
  
  # Store the RMSE for the last round of cross-validation
  results <- rbind(results, data.frame(
    eta = hyper_grid$eta[i],
    max_depth = hyper_grid$max_depth[i],
    nrounds = hyper_grid$nrounds[i],
    RMSE = tail(cv_model$evaluation_log$test_rmse_mean, 1)
  ))
}

# Select the best parameters
best_params <- results[which.min(results$RMSE), ]

# Train the model with the best parameters. Best parameters are extracted and hard-coded.
xgb_best_model <- xgboost(
  data = dtrain,
  params = list(
    objective = "reg:squarederror",
    eta = 0.2,
    max_depth = 1,
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  ),
  nrounds = 250
)

# Prepare the test data
testDataMatrix <- as.matrix(testData[,-which(names(testData) %in% "Rating")])
dtest <- xgb.DMatrix(data = testDataMatrix)

# Make predictions on test data
xgb_preds <- predict(xgb_best_model, dtest)

# Calculate RMSE on test data
rmse <- sqrt(mean((testData$Rating - preds)^2))
print(paste0("Test RMSE: ", rmse))

# Convert the predictors and targets into matrices
trainDataMatrix <- as.matrix(df_model[,-which(names(df_model) %in% "Rating")])
trainLabels <- as.vector(df_model$Rating)

# Convert the data into an xgb.DMatrix object
dtrain <- xgb.DMatrix(data = trainDataMatrix, label = trainLabels)
# Generate predictions for the testing set
xgb_pred <- predict(xgb_best_model, dtrain)

# Create a data frame with the actual and predicted ratings
df <- data.frame(Actual = df_model$Rating,
                 Predicted = xgb_pred )

# Calculate the correlation
correlation <- cor(df$Actual, df$Predicted)

# Create the scatter plot
ggplot(df, aes(x =Predicted , y = Actual)) +
  geom_point() +
  xlab("Predicted Ratings") +
  ylab("Actual Ratings") +
  ggtitle(paste("Correlation:", round(correlation, 2)))+
  xlim(0,5)


standard_error <- sd(df$Actual-df$Predicted)

trainDataMatrix <- as.matrix(potential_CS[,-which(names(potential_CS) %in% "Rating")])
dtrain <- xgb.DMatrix(data = trainDataMatrix)
# Predict for new charging stations
prediction_new<- predict(xgb_best_model, newdata = dtrain)

critical_value <- 1.96
lower_bound <- prediction_new - critical_value * standard_error
upper_bound <- prediction_new + critical_value * standard_error
confidence_interval <- c(lower_bound, upper_bound)

final_xgb<-cbind(potential_CS, Prediction =prediction_new, lower_bound, upper_bound)

###### Model Ensambling#####
library(caret)
library(ranger)
library(gbm)

# Define the parameter grid
parameter_grid <- expand.grid(num.trees = c(50, 100, 150),
                              max.depth = c(2, 3),
                              mtry = c(2, 3),
                              n.trees = c( 50, 100, 150),
                              interaction.depth = c(2, 3),
                              shrinkage = c(0.1))

# Initialize variables for best parameters and RMSE
best_params <- NULL
best_rmse <- Inf

# Perform grid search with k-fold cross-validation
k <- 5  # Number of folds
set.seed(42)  # Set seed for reproducibility

for (i in 1:nrow(parameter_grid)) {
  # Initialize variable for average RMSE across folds
  avg_rmse <- 0
  
  for (fold in 1:k) {
    # Create training and testing indices for the current fold
    indices <- createFolds(trainData$Rating, k = k, list = TRUE, returnTrain = TRUE)
    train_indices <- indices[[fold]]
    test_indices <- indices[[fold]][-fold]
    
    # Split data into training and testing sets for the current fold
    train_data <- trainData[train_indices, ]
    test_data <- trainData[test_indices, ]
    
    # Fit the ranger model with current parameter combination
    ranger_model <- ranger(Rating ~ ., data = train_data, num.trees = parameter_grid$num.trees[i],
                           max.depth = parameter_grid$max.depth[i], mtry = parameter_grid$mtry[i])
    
    # Generate predictions for the testing set using ranger
    ranger_pred <- predict(ranger_model, data = test_data)$predictions
    
    # Fit the gbm model with current parameter combination
    gbm_model <- gbm(Rating ~ ., data = train_data,
                     n.trees = parameter_grid$n.trees[i],
                     interaction.depth = parameter_grid$interaction.depth[i],
                     shrinkage = parameter_grid$shrinkage[i])
    
    # Generate predictions for the testing set using gbm
    gbm_pred <- predict.gbm(gbm_model, newdata = test_data, n.trees = parameter_grid$n.trees[i])
    
    # Combine predictions from both models
    prediction_together <- (ranger_pred + gbm_pred) / 2
    
    # Calculate RMSE for the current fold
    fold_rmse <- sqrt(mean((test_data$Rating - prediction_together)^2))
    
    # Accumulate RMSE across folds
    avg_rmse <- avg_rmse + fold_rmse
  }
  
  # Calculate average RMSE across folds
  avg_rmse <- avg_rmse / k
  
  # Check if current parameter combination is the best
  if (avg_rmse < best_rmse) {
    best_params <- parameter_grid[i, ]
    best_rmse <- avg_rmse
  }
}

# Print the best parameter combination and RMSE
cat("Best Parameters:\n")
print(best_params)
cat("RMSE:", best_rmse)

# Model estimation
ranger_model <- ranger(Rating ~ ., data = trainData, num.trees = 100,
                       max.depth = 3, mtry = 3)
gbm_model <- gbm(Rating ~ ., data = trainData,
                 n.trees = 150,
                 interaction.depth = 3,
                 shrinkage = 0.1)
ranger_pred <- predict(ranger_model, data = testData)$predictions
gbm_pred <- predict.gbm(gbm_model, newdata = testData, n.trees = parameter_grid$n.trees[i])

prediction_together <- (ranger_pred + gbm_pred) / 2
rmse <- sqrt(mean((testData$Rating - prediction_together)^2))

# Create a data frame with the actual and predicted ratings
df <- data.frame(Actual = testData$Rating,
                 Predicted = gbm_pred)

# Calculate the correlation
correlation <- cor(df$Actual, df$Predicted)

# Create the scatter plot
ggplot(df, aes(x =Predicted , y = Actual)) +
  geom_point() +
  xlab("Predicted Ratings") +
  ylab("Actual Ratings") +
  ggtitle(paste("Correlation:", round(correlation, 2)))+
  xlim(0,5)

standard_error <- sd(df$Actual-df$Predicted)

# Predict for new charging stations
ranger_pred <- predict(ranger_model, data = potential_CS)$predictions
gbm_pred <- predict.gbm(gbm_model, newdata =potential_CS, n.trees = parameter_grid$n.trees[i])

prediction_together <- (ranger_pred + gbm_pred) / 2

critical_value <- 1.96
lower_bound <- prediction_new - critical_value * standard_error
upper_bound <- prediction_new + critical_value * standard_error
confidence_interval <- c(lower_bound, upper_bound)

final_GBM<-cbind(potential_CS, Prediction = prediction_new, lower_bound, upper_bound)


# Nothing

##### PCA #####

