
set.seed(123)  # For reproducibility
require(xgboost)
require(caret)

df <- reshaped_poi_locations
splitIndex <- createDataPartition(df$Rating, p = .8, 
                                  list = FALSE, 
                                  times = 1)

trainData <- df[ splitIndex,]
testData  <- df[-splitIndex,]

# Convert the predictors and targets into matrices
trainDataMatrix <- as.matrix(trainData[,-which(names(trainData) %in% "Rating")])
trainLabels <- as.vector(trainData$Rating)

# Convert the data into an xgb.DMatrix object
dtrain <- xgb.DMatrix(data = trainDataMatrix, label = trainLabels)

# Set up the parameters for xgboost
params <- list(
  objective = "reg:squarederror", 
  eta = 0.3,
  max_depth = 3,
  min_child_weight = 1,
  subsample = 1,
  colsample_bytree = 1
)

# Define the grid
hyper_grid <- expand.grid(
  eta = c(0.1, 0.3, 0.5),
  max_depth = c(1, 2, 3),
  nrounds = c(50, 100, 250, 500)
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

# Train the model with the best parameters
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
testDataMatrix <- as.matrix(testData[,-which(names(testData) %in% "Rating")])
dtest <- xgb.DMatrix(data = testDataMatrix)

# Make predictions on test data
preds <- predict(xgb_best_model, dtest)

# Calculate RMSE on test data
rmse <- sqrt(mean((testData$Rating - preds)^2))
print(paste0("Test RMSE: ", rmse))
