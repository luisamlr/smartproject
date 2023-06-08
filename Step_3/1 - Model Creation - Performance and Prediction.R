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

# Set the working directory to the location of the script.
# On RStudio, the code below can be used. If not, please specify manually the folder where this script is located.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("/path/to/your/script")

##### Model  DATA preparation#### 

# Import dataset
df_model <- read_excel("source_data/reshaped_poi_locations.xlsx")
potential_CS <- read_excel("source_data/reshaped_poi_locations_potential.xlsx")

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
validationData  <- df_model[-splitIndex,] # Validation dataset

# This part of the code is dedicated to tuning different models and finding the best.
# By default, you will skip this section and go straight to the final model.
# You can set the skip_code in the next line to FALSE to run all models and comparisons.
skip_code <- FALSE  # set to FALSE if you want to run the code block

if (!skip_code) {

###### GBM - Model and estimation#####
# Define the parameter grid
parameter_grid <- expand.grid(n.trees = c(25, 50, 75, 100, 125, 150),
                              # Specifies the number of trees (base learners) 
                              # in the gradient boosting model. 
                              interaction.depth = c(2, 3, 4),
                              # Controls the depth of each tree in the ensemble.
                              # A higher value capture more complex interactions 
                              # between features. 
                              shrinkage = c(0.1, 0.2, 0.3), 
                              #Shrinkage, determines the contribution of each 
                              # tree to the final prediction. A smaller value 
                              # makes the model more conservative, requiring 
                              # more trees for convergence but potentially 
                              # improving generalization.
                              n.minobsinnode = c(5,10)) 
                              # Minimum observations final node

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
                 n.trees = gbm_model$bestTune[1],
                 interaction.depth = gbm_model$bestTune[2],
                 shrinkage = gbm_model$bestTune[3])

# Generate predictions for the Validation set (this has not been trained or tested yet)
gbm_pred <- predict(gbm_model, newdata = validationData, n.trees = 25)
gmb_v_rmse <- sqrt(mean((validationData$Rating - gbm_pred)^2))
print(paste0("Validation RMSE: ", gmb_v_rmse))
# Calculate MAE on test data
gmb_v_mae <- mean(abs(validationData$Rating - gbm_pred))
print(paste0("Validation MAE: ", gmb_v_mae))

# Create a data frame with the actual and predicted ratings
df <- data.frame(Actual = validationData$Rating,
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
  ggtitle(paste("Correlation (GBM):", round(correlation_gmb, 2))) +
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
                       num.trees = best_params$num.trees,
                       max.depth = best_params$max.depth,
                       mtry = best_params$mtry)

# Generate predictions for the testing set
ranger_pred <- predict(ranger_model, data = validationData)$predictions
ranger_v_rmse <- sqrt(mean((validationData$Rating - ranger_pred)^2))
print(paste0("Validation RMSE: ", ranger_v_rmse))
# Calculate MAE on test data
ranger_v_mae <- mean(abs(validationData$Rating - ranger_pred))
print(paste0("Validation MAE: ", ranger_v_mae ))

# Create a data frame with the actual and predicted ratings
df <- data.frame(Actual = validationData$Rating,
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
  ggtitle(paste("Correlation (RF):", round(correlation_ranger , 2))) +
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
xgb_best_params <- results[which.min(results$RMSE), ]
print(xgb_best_params)
# Train the model with the best parameters. Best parameters are extracted.
xgb_best_model <- xgboost(
  data = dtrain,
  params = list(
    objective = "reg:squarederror", # The loss function to minimize. In this case, it's the squared error used for regression problems.
    eta = xgb_best_params$eta, # The learning rate, determining how quickly the model learns. It helps to prevent overfitting.
    max_depth = xgb_best_params$max_depth, #The maximum depth of any given tree within the model, controlling the complexity of the model.
    gamma = 0, # The minimum loss reduction required to make a split, acting as a regularization parameter.
    colsample_bytree = 1, # The fraction of columns (features) to be randomly sampled for each tree.
    min_child_weight = 1, # The minimum sum of instance (hessian) weight needed in a child (leaf).
    subsample = 1 # The fraction of observations (rows) to be randomly sampled for each tree.
  ),
  nrounds = xgb_best_params$nrounds # The number of boosting rounds or trees to build. This is effectively the subsequent models built after calculating residuals.
)

# Prepare the test data
validationDataMatrix <- as.matrix(validationData[,-which(names(validationData) %in% "Rating")])
dtest <- xgb.DMatrix(data = validationDataMatrix)

# Make predictions on test data
xgb_pred <- predict(xgb_best_model, dtest)

# Calculate RMSE on test data
xgb_v_rmse <- sqrt(mean((validationData$Rating - xgb_pred)^2))
print(paste0("Validation RMSE: ", xgb_v_rmse))
# Calculate MAE on test data
xgb_v_mae <- mean(abs(validationData$Rating - xgb_pred))
print(paste0("Validation MAE: ", xgb_v_mae ))

# Create a data frame with the actual and predicted ratings
df <- data.frame(Actual = validationData$Rating,
                 Predicted = xgb_pred )

# Calculate the correlation
correlation_xgb <- cor(df$Actual, df$Predicted)
mc_xgb<-c(xgb_v_rmse, xgb_v_mae, correlation_xgb)
standard_error_xgb <- sd(df$Actual-df$Predicted)

# Create the scatter plot
ggplot(df, aes(x = Predicted, y = Actual)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Add the diagonal line
  xlab("Predicted Ratings") +
  ylab("Actual Ratings") +
  ggtitle(paste("Correlation (XGB):", round(correlation_xgb, 2))) +
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
ranger_model <- ranger(Rating ~ ., data = trainData, num.trees = best_params$num.trees,
                       max.depth = best_params$max.depth, mtry = best_params$mtry)
gbm_model <- gbm(Rating ~ ., data = trainData,
                 n.trees = best_params$n.trees,
                 interaction.depth = best_params$interaction.depth,
                 shrinkage = best_params$shrinkage)
ranger_pred <- predict(ranger_model, data = validationData)$predictions
gbm_pred <- predict.gbm(gbm_model, newdata = validationData, n.trees = parameter_grid$n.trees[i])

# Calculate RMSE on test data
prediction_together <- (ranger_pred + gbm_pred) / 2
ens_v_rmse <- sqrt(mean((validationData$Rating - prediction_together)^2))
print(paste0("Test RSME: ", ens_v_rmse))
# Calculate MAE on test data
ens_v_mae <- mean(abs(validationData$Rating - prediction_together))
print(paste0("Test MAE: ", ens_v_mae ))

# Create a data frame with the actual and predicted ratings
df <- data.frame(Actual = validationData$Rating,
                 Predicted = prediction_together)

# Calculate the correlation
correlation_ens <- cor(df$Actual, df$Predicted)
mc_ens<-c(ens_v_rmse, ens_v_mae, correlation_ens)

# Create the scatter plot
ggplot(df, aes(x =Predicted , y = Actual)) +
  geom_point() +
  xlab("Predicted Ratings") +
  ylab("Actual Ratings") +
  ggtitle(paste("Correlation:", round(correlation_ens, 2)))+
  xlim(0,5)

##### PCA #####

target_var <- "Rating"


# To keep the original dataset
training_data <- trainData
testing_data <- validationData

# Separate features and target variable
training_features <- training_data[, !(names(training_data) %in% target_var)]
training_target <- training_data[[target_var]]

testing_features <- testing_data[, !(names(testing_data) %in% target_var)]
testing_target <- testing_data[[target_var]]

# Check for columns with zero variance in the training data
zero_var_cols <- nearZeroVar(training_features, saveMetrics = TRUE)

# Keep only columns with variance not equal to zero
training_features <- training_features[, zero_var_cols$nzv == FALSE]
testing_features <- testing_features[, names(testing_features) %in% names(training_features)]

# Standardize the training data
training_features <- scale(training_features)

# Save center and scale of the training data
training_center <- attr(training_features, "scaled:center")
training_scale <- attr(training_features, "scaled:scale")

# Fit PCA on the training data
pca_model <- prcomp(training_features)


# Identify the number of principal components needed to explain at least 80% of the variance
explained_variance_ratio <- pca_model$sdev^2 / sum(pca_model$sdev^2)
cumulative_explained_variance <- cumsum(explained_variance_ratio)
num_components <- length(explained_variance_ratio)

# Create a plot of cumulative explained variance
plot(1:num_components, cumulative_explained_variance, type = "b", 
     xlab = "Number of Components", ylab = "Cumulative Explained Variance",
     main = "Cumulative Variance Explained by Principal Components", pch = 19)

# Add a horizontal line at 80% cumulative explained variance
abline(h = 0.8, col = "red", lty = 2)

# Add a legend
legend("bottomright", legend = c("Cumulative Explained Variance", "80% Threshold"),
       col = c("black", "red"), lty = c(1, 2), pch = c(19, NA))

num_components <- which(cumulative_explained_variance >= 0.80)[1]


# Now let's apply this transformation to the test data.
# First, we need to standardize the test data using the mean and sd of the training data.
testing_features <- scale(testing_features, center = training_center, scale = training_scale)

# Now transform the test data using the PCA model
transformed_test_features <- predict(pca_model, newdata = testing_features)
# Retain only the necessary principal components in the transformed training and test data
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

# Train the model with the best parameters. Extracted and hard-coded.
xgb_best_model <- xgboost(
  data = dtrain,
  params = list(
    objective = "reg:squarederror",
    eta = best_params$eta,
    max_depth = best_params$max_depth,
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  ),
  nrounds = best_params$nrounds
)

# Prepare the test data
validationDataMatrix <- as.matrix(transformed_testing_data[,-which(names(transformed_testing_data) %in% "Rating")])
dtest <- xgb.DMatrix(data = validationDataMatrix)

# Make predictions on test data
preds <- predict(xgb_best_model, dtest)

# Calculate RMSE on test data
pca_v_rmse <- sqrt(mean((transformed_testing_data$Rating - preds)^2))
pca_v_rmse

# Calculate MAE on test data
pca_v_mae <- mean(abs(transformed_testing_data$Rating - preds))
print(paste0("Validation MAE: ", pca_v_mae))

# Create a data frame with the actual and predicted ratings
df <- data.frame(Actual = transformed_testing_data$Rating,
                 Predicted = preds)

# Calculate the correlation
correlation_pca <- cor(df$Actual, df$Predicted)

mc_pca<-c(pca_v_rmse, pca_v_mae, correlation_pca)

# Create the scatter plot
ggplot(df, aes(x =Predicted , y = Actual)) +
  geom_point() +
  xlab("Predicted Ratings") +
  ylab("Actual Ratings") +
  ggtitle(paste("Correlation:", round(correlation_pca, 2)))+
  xlim(0,5)


###### Model Comparison and Selection#####

model_comparison<-cbind(GBM = mc_gmb, RF = mc_ranger, XGB = mc_xgb, "ENS(RF/GBM)" = mc_ens, PCA = mc_pca)
model_comparison
}

# Select XGB as the most accurate and efficient model

# Train the model with the best parameters. Best parameters are extracted.
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
# Train the model with the best parameters. Best parameters are extracted.
xgb_best_model <- xgboost(
  data = dtrain,
  params = list(
    objective = "reg:squarederror",
    eta = best_params$eta,
    max_depth = best_params$max_depth,
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  ),
  nrounds = best_params$nrounds
)

# Prepare the test data
validationDataMatrix <- as.matrix(validationData[,-which(names(validationData) %in% "Rating")])
dtest <- xgb.DMatrix(data = validationDataMatrix)

# Make predictions on test data
xgb_pred <- predict(xgb_best_model, dtest)

# Calculate RMSE on test data
xgb_v_rmse <- sqrt(mean((validationData$Rating - xgb_pred)^2))
print(paste0("Validation RMSE: ", xgb_v_rmse))
# Calculate MAE on test data
xgb_v_mae <- mean(abs(validationData$Rating - xgb_pred))
print(paste0("Validation MAE: ", xgb_v_mae ))

# Create a data frame with the actual and predicted ratings
df <- data.frame(Actual = validationData$Rating,
                 Predicted = xgb_pred )

# Calculate the correlation
correlation_xgb <- cor(df$Actual, df$Predicted)
mc_xgb<-c(xgb_v_rmse, xgb_v_mae, correlation_xgb)
standard_error_xgb <- sd(df$Actual-df$Predicted)
margin_of_error <- 1.96 * standard_error_xgb
df$UpperBound <- df$Predicted + margin_of_error
df$LowerBound <- df$Predicted - margin_of_error
df$WithinBounds <- df$Actual >= df$LowerBound & df$Actual <= df$UpperBound
coverage_ratio_xgb_95 <- sum(df$WithinBounds) / nrow(df)
margin_of_error <- 1.645 * standard_error_xgb
df$UpperBound <- df$Predicted + margin_of_error
df$LowerBound <- df$Predicted - margin_of_error
df$WithinBounds <- df$Actual >= df$LowerBound & df$Actual <= df$UpperBound
coverage_ratio_xgb_90 <- sum(df$WithinBounds) / nrow(df)
margin_of_error <- 0.674 * standard_error_xgb
df$UpperBound <- df$Predicted + margin_of_error
df$LowerBound <- df$Predicted - margin_of_error
df$WithinBounds <- df$Actual >= df$LowerBound & df$Actual <= df$UpperBound
coverage_ratio_xgb_75 <- sum(df$WithinBounds) / nrow(df)

# Create the scatter plot
ggplot(df, aes(x = Predicted, y = Actual)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Add the diagonal line
  xlab("Predicted Ratings") +
  ylab("Actual Ratings") +
  ggtitle(paste("Correlation (XGB):", round(correlation_xgb, 2))) +
  xlim(1, 5)


trainDataMatrix <- as.matrix(potential_CS[,-which(names(potential_CS) %in% "Rating")])
dtrain <- xgb.DMatrix(data = trainDataMatrix)

# Predict for new charging stations
prediction_new<- predict(xgb_best_model, newdata = dtrain)

critical_value <- 1.96
lower_bound_95 <- prediction_new - critical_value * standard_error_xgb
upper_bound_95 <- prediction_new + critical_value * standard_error_xgb

critical_value <- 1.036 
lower_bound_75 <- prediction_new - critical_value * standard_error_xgb
upper_bound_75 <- prediction_new + critical_value * standard_error_xgb

# Set the boundaries
lower_bound <- 0
upper_bound <- 5

# Apply the boundaries to the predictions
prediction_new <- pmax(pmin(prediction_new, upper_bound), lower_bound)
lower_bound_95 <- pmax(pmin(lower_bound_95, upper_bound), lower_bound)
upper_bound_95 <- pmax(pmin(upper_bound_95, upper_bound), lower_bound)
lower_bound_75 <- pmax(pmin(lower_bound_75, upper_bound), lower_bound)
upper_bound_75 <- pmax(pmin(upper_bound_75, upper_bound), lower_bound)

final_xgb<-cbind(potential_CS, Prediction =prediction_new, lower_bound_95, upper_bound_95,  lower_bound_75, upper_bound_75)
# Plotting the histogram of new predictions
ggplot(final_xgb, aes(x = prediction_new)) +
  geom_histogram(binwidth = 0.10, color = "black", fill = "lightblue") +
  xlim(0, 5) +
  labs(title = "Charging Station Ratings (XGBoost)") 

# Variable importance
importance_scores <- xgb.importance(feature_names = names(trainDataMatrix ), 
                                    model = xgb_best_model)

# View the importance scores
print(importance_scores)

# Export data frame to an Excel file
write_xlsx(final_xgb, "output/potential_CS_final.xlsx") 
# Export data frame to an Excel file
write_xlsx(importance_scores, "output/variable_importance.xlsx") 
