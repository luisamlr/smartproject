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

# Tranform dataset
result <- add_missing_columns(df_model, potential_CS)
potential_CS <- result$df2
df_model <- result$df1

# Separet the dataset into training-testing and validation
splitIndex <- createDataPartition(df_model$Rating, p = .9, 
                                  list = FALSE, 
                                  times = 1)

trainData <- df_model[ splitIndex,]
testData  <- df_model[-splitIndex,]


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
k <- 10  # Number of folds
set.seed(42)  # Set seed for reproducibility

for (i in 1:nrow(parameter_grid)) {
  # Initialize variable for average RMSE across folds
  avg_rmse <- 0
  
  for (fold in 1:k) {
    # Create training and testing indices for the current fold
    indices <- createFolds(df_model$Rating, k = k, list = TRUE)
    train_indices <- unlist(indices[-fold])
    test_indices <- indices[[fold]]
    
    # Split data into training and testing sets for the current fold
    train_data <- df_model[train_indices, ]
    test_data <- df_model[test_indices, ]
    
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

# Define the parameter grid
parameter_grid <- expand.grid(mtry = c(2,3,4,5,6,7,8,9))

# Define the control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Perform grid search with cross-validation using caret
rf_model <- train(Rating ~ ., data = trainData, method = "rf",
                   trControl = ctrl, tuneGrid = parameter_grid)



# Define the parameter grid
parameter_grid <- expand.grid(mtry = c(2, 3, 4),
                              num.trees = c(50, 100, 150),
                              max.depth = c(2, 3, 4),
                              min.node.size = c(1, 5, 10))

# Define the control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Perform grid search with cross-validation using caret
rf_model <- train(Rating ~ ., data = trainData, method = "rf",
                  trControl = ctrl, tuneGrid = parameter_grid)


# Fit the model with current parameter combination
ranger_model <- ranger(Rating ~ ., data = df_model,
                    num.trees = 150,
                    max.depth = 3)

# Generate predictions for the testing set
ranger_pred <- predict(ranger_model, data = df_model)$predictions

# Variable importance

# Create a data frame with the actual and predicted ratings
df <- data.frame(Actual = df_model$Rating,
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


###### SVM - Model and estimation####
# Define the parameter grid
parameterGrid <- expand.grid(
       # Cost values
  .kernel = c("linear", "radial")
)

# Initialize variables for best parameters and RMSE
best_params <- NULL
best_rmse <- Inf
rmse_comb <- c()

# Perform grid search with k-fold cross-validation
k <- 10  # Number of folds

for (i in 1:nrow(parameterGrid)) {
  # Initialize variable for average RMSE across folds
  avg_rmse <- 0
  
  for (fold in 1:k) {
    # Create training and testing indices for the current fold
    indices <- createFolds(df_model$Rating, k = k, list = TRUE)
    train_indices <- unlist(indices[-fold])
    test_indices <- indices[[fold]]
    
    # Split data into training and testing sets for the current fold
    train_data <- df_model[train_indices, ]
    test_data <- df_model[test_indices, ]
    
    # Fit the model with current parameter combination
    svm_model <- svm(Rating ~ ., data = train_data,
                     kernel = parameterGrid$.kernel[i])
    
    # Generate predictions for the testing set
    svm_pred <- predict(svm_model, newdata = test_data)
    
    # Calculate RMSE for the current fold
    fold_rmse <- sqrt(mean((test_data$Rating - svm_pred)^2))
    
    # Accumulate RMSE across folds
    avg_rmse <- avg_rmse + fold_rmse
  }
  
  # Calculate average RMSE across folds
  avg_rmse <- avg_rmse / k
  rmse_comb[i] <- avg_rmse
  
  # Check if current parameter combination is the best
  if (avg_rmse < best_rmse) {
    best_params <- parameterGrid[i, ]
    best_rmse <- avg_rmse
  }
}

# Print the best parameter combination and RMSE
cat("Best Parameters:\n")
print(best_params)
cat("RMSE:", best_rmse)


# Fit the model with current parameter combination
svm_model <- svm(Rating ~ ., data = df_model,
                 n.trees = 50,
                 interaction.depth = 3,
                 shrinkage = 0.1)

# Generate predictions for the testing set
svm_pred <- predict(svm_model, newdata = df_model, n.trees = 50)

temp<-summary.svm(svm_model)
plot(temp$rel.inf)

# Create a data frame with the actual and predicted ratings
df <- data.frame(Actual = df_model$Rating,
                 Predicted = svm_pred)

# Calculate the correlation
correlation <- cor(df$Actual, df$Predicted)

# Create the scatter plot
ggplot(df, aes(x =Predicted , y = Actual)) +
  geom_point() +
  xlab("Predicted Ratings") +
  ylab("Actual Ratings") +
  ggtitle(paste("Correlation:", round(correlation, 2)))+
  xlim(0,5)+
  abline(01)

standard_error <- sd(df$Actual-df$Predicted)

# Predict for new charging stations
prediction_new<- predict(svm_model, newdata = potential_CS, n.trees = 50)

critical_value <- 1.96
lower_bound <- prediction_new - critical_value * standard_error
upper_bound <- prediction_new + critical_value * standard_error
confidence_interval <- c(lower_bound, upper_bound)

final_SVM<-cbind(potential_CS, Prediction =prediction_new, lower_bound, upper_bound)

###### XGB - Model and estimation####

##### PCA #####

