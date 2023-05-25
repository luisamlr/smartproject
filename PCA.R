set.seed(42)  # For reproducibility
require(xgboost)
require(caret)

df <- reshaped_poi_locations

# Identify columns with zero variance
zero_var_cols <- apply(df, 2, var) == 0

# Drop these columns
df <- df[, !zero_var_cols]

predictors <- df[, !(names(df) %in% "Rating")]
target <- df$Rating

# Standardize the predictors
predictors <- scale(predictors)

# Perform PCA
pca <- prcomp(predictors, center = TRUE, scale. = TRUE)

# Determine the number of components needed to explain 90% of the variance
explained_variance <- summary(pca)$importance[2,]
plot(explained_variance)
num_components <- which(cumsum(explained_variance) >= 0.70)[1]
# Since we need 84 PCs to capture at least 70% of the variance, using PCs doesn't look helpful

PCs <- pca$x[, 1:num_components]
# Convert the PCs into a data frame
PCs_df <- as.data.frame(PCs)

df <- cbind(PCs_df, target)
colnames(df)[85] <- "Rating"
splitIndex <- createDataPartition(df$Rating, p = .9, 
                                  list = FALSE, 
                                  times = 1)

trainData <- df[ splitIndex,]
testData  <- df[-splitIndex,]

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
testDataMatrix <- as.matrix(testData[,-which(names(testData) %in% "Rating")])
dtest <- xgb.DMatrix(data = testDataMatrix)

# Make predictions on test data
preds <- predict(xgb_best_model, dtest)

# Calculate RMSE on test data
rmse <- sqrt(mean((testData$Rating - preds)^2))
print(paste0("Test RMSE: ", rmse))
## RMSE is 1.569