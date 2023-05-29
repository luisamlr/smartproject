# ------------------------------------------------------------------------------
# We compare the performance of GBM, Random Forest, XGBoost, and an ensemble of 
# GBM and Random Forest models in estimating current charging station ratings, 
# as well as the consideration of PCA in improving the XGBoost model.  
# The evaluation involves a cross-validation approach, where 10% of the dataset 
# is kept as validation and the remaining 90% is used for model tuning via 
# 5-fold cross-validation.
# The caret package is utilized, with the RMSE (Root Mean Square Error) serving 
# as the primary metric for tuning the models. Additionally, other metrics such 
# as MAE (Mean Absolute Error) and correlation ratio are considered for model 
# comparison using the 10% validation set.
# Finally, new predictions are generated for potential charging stations and 
# exported with 95% and 70% confidence intervals.
# ------------------------------------------------------------------------------

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
potential_CS <- read_excel("reshaped_poi_locations_potential.xlsx")

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
                 interaction.depth = 3,
                 shrinkage = 0.1)

# Generate predictions for the Validation set (this has not been trained or tested yet)
gbm_pred <- predict(gbm_model, newdata = testData, n.trees = 25)
gmb_v_rmse <- sqrt(mean((testData$Rating - gbm_pred)^2))
print(paste0("Validation RMSE: ", gmb_v_rmse))
# Calculate MAE on test data
gmb_v_mae <- mean(abs(testData$Rating - gbm_pred))
print(paste0("Validation MAE: ", gmb_v_mae))

# Create a data frame with the actual and predicted ratings
df <- data.frame(Actual = testData$Rating,
                 Predicted = gbm_pred)

# Calculate the correlation
correlation_gmb <- cor(df$Actual, df$Predicted)

mc_gmb<-c(gmb_v_rmse, gmb_v_mae, correlation_gmb)

# Create the scatter plot
ggplot(df, aes(x = Predicted, y = Actual)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Add the diagonal line
  xlab("Predicted Ratings") +
  ylab("Actual Ratings") +
  ggtitle(paste("Correlation (GBM):", round(correlation, 2))) +
  xlim(1, 5)

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
                       max.depth = 3,
                       mtry = 2)

# Generate predictions for the testing set
ranger_pred <- predict(ranger_model, data = testData)$predictions
ranger_v_rmse <- sqrt(mean((testData$Rating - ranger_pred)^2))
print(paste0("Validation RMSE: ", ranger_v_rmse))
# Calculate MAE on test data
ranger_v_mae <- mean(abs(testData$Rating - ranger_pred))
print(paste0("Validation MAE: ", ranger_v_mae ))

# Create a data frame with the actual and predicted ratings
df <- data.frame(Actual = testData$Rating,
                 Predicted = ranger_pred)

# Calculate the correlation
correlation_ranger <- cor(df$Actual, df$Predicted)
mc_ranger<-c(ranger_v_rmse, ranger_v_mae, correlation_ranger)

# Create the scatter plot
ggplot(df, aes(x = Predicted, y = Actual)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Add the diagonal line
  xlab("Predicted Ratings") +
  ylab("Actual Ratings") +
  ggtitle(paste("Correlation (RF):", round(correlation, 2))) +
  xlim(1, 5)

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

params<-c()
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
print(best_params)
# Train the model with the best parameters. Best parameters are extracted and hard-coded.
xgb_best_model <- xgboost(
  data = dtrain,
  params = list(
    objective = "reg:squarederror",
    eta = 0.1,
    max_depth = 1,
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  ),
  nrounds = 100
)

# Prepare the test data
testDataMatrix <- as.matrix(testData[,-which(names(testData) %in% "Rating")])
dtest <- xgb.DMatrix(data = testDataMatrix)

# Make predictions on test data
xgb_pred <- predict(xgb_best_model, dtest)

# Calculate RMSE on test data
xgb_v_rmse <- sqrt(mean((testData$Rating - xgb_pred)^2))
print(paste0("Validation RMSE: ", xgb_v_rmse))
# Calculate MAE on test data
xgb_v_mae <- mean(abs(testData$Rating - xgb_pred))
print(paste0("Validation MAE: ", xgb_v_mae ))

# Create a data frame with the actual and predicted ratings
df <- data.frame(Actual = testData$Rating,
                 Predicted = xgb_pred )

# Calculate the correlation
correlation_xgb <- cor(df$Actual, df$Predicted)
mc_xgb<-c(xgb_v_rmse, xgb_v_mae, correlation_xgb)

# Create the scatter plot
ggplot(df, aes(x = Predicted, y = Actual)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Add the diagonal line
  xlab("Predicted Ratings") +
  ylab("Actual Ratings") +
  ggtitle(paste("Correlation (GBM):", round(correlation, 2))) +
  xlim(1, 5)

###### Model Ensembling#####
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
                       max.depth = 3, mtry = 2)
gbm_model <- gbm(Rating ~ ., data = trainData,
                 n.trees = 150,
                 interaction.depth = 3,
                 shrinkage = 0.1)
ranger_pred <- predict(ranger_model, data = testData)$predictions
gbm_pred <- predict.gbm(gbm_model, newdata = testData, n.trees = parameter_grid$n.trees[i])

# Calculate RMSE on test data
prediction_together <- (ranger_pred + gbm_pred) / 2
ens_v_rmse <- sqrt(mean((testData$Rating - prediction_together)^2))
print(paste0("Test RSME: ", ens_v_rmse))
# Calculate MAE on test data
ens_v_mae <- mean(abs(testData$Rating - prediction_together))
print(paste0("Test MAE: ", ens_v_mae ))

# Create a data frame with the actual and predicted ratings
df <- data.frame(Actual = testData$Rating,
                 Predicted = prediction_together)

# Calculate the correlation
correlation_ens <- cor(df$Actual, df$Predicted)
mc_ens<-c(ens_v_rmse, ens_v_mae, correlation_ens)

# Create the scatter plot
ggplot(df, aes(x =Predicted , y = Actual)) +
  geom_point() +
  xlab("Predicted Ratings") +
  ylab("Actual Ratings") +
  ggtitle(paste("Correlation:", round(correlation, 2)))+
  xlim(0,5)

##### PCA #####

target_var <- "Rating"


# To keep the original dataset
training_data <- trainData
testing_data <- testData

# Separate features and target variable
training_features <- training_data[, !(names(training_data) %in% target_var)]
training_target <- training_data[[target_var]]

testing_features <- testing_data[, !(names(testing_data) %in% target_var)]
testing_target <- testing_data[[target_var]]


# Determine the explained variance for each principal component
explained_variance <- summary(pca)$importance[2, ]
num_components <- length(explained_variance)

# Calculate the cumulative sum of explained variance
cumulative_variance <- cumsum(explained_variance)

# Create a plot of cumulative explained variance
plot(1:num_components, cumulative_variance, type = "b", 
     xlab = "Number of Components", ylab = "Cumulative Explained Variance",
     main = "Cumulative Variance Explained by Principal Components", pch = 19)

# Add a horizontal line at 90% cumulative explained variance
abline(h = 0.9, col = "red", lty = 2)

# Add a legend
legend("bottomright", legend = c("Cumulative Explained Variance", "90% Threshold"),
       col = c("black", "red"), lty = c(1, 2), pch = c(19, NA))


num_components <- which(cumsum(explained_variance) >= 0.90)[1]
# Since we need 191 PCs to capture at least 90% of the variance, using PCs doesn't look helpful

# Check for columns with zero variance in the training data
zero_var_cols <- nearZeroVar(training_features, saveMetrics = TRUE)

# Keep only columns with variance not equal to zero
training_features <- training_features[, zero_var_cols$nzv == FALSE]
testing_features <- testing_features[, names(testing_features) %in% names(training_features)]


df <- cbind(PCs_df, target)
colnames(df)[192] <- "Rating"
splitIndex <- createDataPartition(df$Rating, p = .9, 
                                  list = FALSE, 
                                  times = 1)

# Standardize the training data
training_features <- scale(training_features)


# Save center and scale of the training data
training_center <- attr(training_features, "scaled:center")
training_scale <- attr(training_features, "scaled:scale")

# Fit PCA on the training data
pca_model <- prcomp(training_features)

# Print summary of the PCA model
print(summary(pca_model))

# Identify the number of principal components needed to explain at least 90% of the variance
explained_variance_ratio <- pca_model$sdev^2 / sum(pca_model$sdev^2)
cumulative_explained_variance <- cumsum(explained_variance_ratio)
num_components <- which(cumulative_explained_variance >= 0.90)[1]

# Now let's apply this transformation to the test data.
# First, we need to standardize the test data using the mean and sd of the training data.
testing_features <- scale(testing_features, center = training_center, scale = training_scale)

# Now transform the test data using the PCA model
transformed_test_features <- predict(pca_model, newdata = testing_features)
# Add back the target variable
transformed_training_features <- pca_model$x[, 1:num_components]
transformed_test_features <- transformed_test_features[, 1:num_components]

# Add back the target variable
transformed_training_data <- data.frame(transformed_training_features, Rating = training_target)
transformed_testing_data <- data.frame(transformed_test_features, Rating = testing_target)

# Convert the predictors and targets into matrices
trainDataMatrix <- as.matrix(transformed_training_data[,-which(names(transformed_training_data) %in% "Rating")])
trainLabels <- as.vector(transformed_training_data$Rating)

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
best_params

# Train the model with the best parameters. Extracted and hard-coded.
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
  nrounds = 70
)

# Prepare the test data
testDataMatrix <- as.matrix(transformed_testing_data[,-which(names(transformed_testing_data) %in% "Rating")])
dtest <- xgb.DMatrix(data = testDataMatrix)

# Make predictions on test data
preds <- predict(xgb_best_model, dtest)

# Calculate RMSE on test data
pca_v_rmse <- sqrt(mean((testData$Rating - preds)^2))
pca_v_rmse
# Calculate MAE on test data
pca_v_mae <- mean(abs(testData$Rating - preds))
print(paste0("Validation MAE: ", pca_v_mae))

# Create a data frame with the actual and predicted ratings
df <- data.frame(Actual = testData$Rating,
                 Predicted = preds)

# Calculate the correlation
correlation_pca <- cor(df$Actual, df$Predicted)

mc_pca<-c(pca_v_rmse, pca_v_mae, correlation_pca)

# Create the scatter plot
ggplot(df, aes(x =Predicted , y = Actual)) +
  geom_point() +
  xlab("Predicted Ratings") +
  ylab("Actual Ratings") +
  ggtitle(paste("Correlation:", round(correlation, 2)))+
  xlim(0,5)

###### Model Comparison and Selection#####

model_comparison<-cbind(GBM = mc_gmb, RF = mc_ranger, XGB = mc_xgb, "ENS(RF/GBM)" = mc_ens, PCA = mc_pca)
model_comparison


