# Install and load the required packages
library(caTools)
library(randomForest)
library(dplyr)
library(readxl)
library(reshape2)
library(openxlsx)
library(writexl)
library(ranger)
library(caret)
library(e1071)

# Loading chargers information.
ev_chargers_reviews <- read.csv("All_Chargers.csv")
poi_locations <- read_excel("facilities_around_coordinates.xlsx")
demog_data <- read_excel("CBS.xlsx")
parking_maastricht <- read_excel("parking_spots_maastricht.xlsx")

# Eliminate duplicates
ev_chargers_reviews <- distinct(ev_chargers_reviews)
poi_locations <- distinct(poi_locations)
demog_data <- distinct(demog_data)
parking_maastricht <- distinct(parking_maastricht)

# Format corrections for necessary variables.
poi_locations$latitude <- round(as.numeric(poi_locations$latitude),7)
poi_locations$longitude <- round(as.numeric(poi_locations$longitude),7)
poi_locations$charger_latitude <- round(as.numeric(poi_locations$charger_latitude),7)
poi_locations$charger_longitude <- round(as.numeric(poi_locations$charger_longitude),7)
ev_chargers_reviews$Latitude <- round(as.numeric(ev_chargers_reviews$Latitude),7)
ev_chargers_reviews$Longitude <- round(as.numeric(ev_chargers_reviews$Longitude),7)
parking_maastricht$latitude <- round(as.numeric(parking_maastricht$latitude),7)
parking_maastricht$longitude <- round(as.numeric(parking_maastricht$longitude),7)
demog_data$StringValue <- as.character(demog_data$StringValue)

# Function to calculate distance between two set of coordinates.
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Convert latitude and longitude from degrees to radians
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Calculate Haversine distance
  a <- sin((lat2 - lat1) / 2)^2 + cos(lat1) * cos(lat2) * sin((lon2 - lon1) / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6371 # Earth's mean radius in kilometers (corrected)
  d <- R * c
  
  return(d)
}

poi_locations$distance <- mapply(haversine_distance, poi_locations$charger_latitude, poi_locations$charger_longitude, poi_locations$latitude, poi_locations$longitude)
poi_locations$distance <- as.numeric(poi_locations$distance)

# Ensure that the key columns in both data frames have the same names
names(ev_chargers_reviews)[names(ev_chargers_reviews) == "Longitude"] <- "charger_longitude"
names(ev_chargers_reviews)[names(ev_chargers_reviews) == "Latitude"] <- "charger_latitude"

names(demog_data)[names(demog_data) == "Longitude"] <- "charger_longitude"
names(demog_data)[names(demog_data) == "Latitude"] <- "charger_latitude"

# Reshaping the table
reshaped_poi_locations <- dcast(poi_locations, charger_longitude + charger_latitude ~ type, value.var = "distance")

# Adding charger information
reshaped_poi_locations <- reshaped_poi_locations %>%
  left_join(ev_chargers_reviews, by = c("charger_longitude" = "charger_longitude", "charger_latitude" = "charger_latitude"))

# Trimed postal code for cbs merge
reshaped_poi_locations$postal_trim <- substr(reshaped_poi_locations$`Postal.Code`, start = 1, stop = 4)

# Left join the demog_data
reshaped_poi_locations <- reshaped_poi_locations %>%
  left_join(demog_data, by = c("postal_trim"="StringValue"))

# Clean one column that is completely null
reshaped_poi_locations <- reshaped_poi_locations[, !(colnames(reshaped_poi_locations) == "WijkenEnBuurten")]

# Checking and fixing variable names
names(reshaped_poi_locations) <- make.names(names(reshaped_poi_locations))
reshaped_poi_locations <- na.omit(reshaped_poi_locations)

## Random Forest Model ##

# Split the data into training (70%) and testing (30%) sets
sample_size <- floor(0.7 * nrow(reshaped_poi_locations))
train_idx <- sample(seq_len(nrow(reshaped_poi_locations)), size = sample_size)
train_data <- reshaped_poi_locations[train_idx, ]
test_data <- reshaped_poi_locations[-train_idx, ]

# Build the model using the training data
rf_model <- ranger(Rating ~ ., data = train_data, importance = "permutation")

# Extract the variable importance
importance <- rf_model$variable.importance

# Create a 10-fold cross-validation scheme
cv_scheme <- createFolds(train_data$Rating, k = 10)

# Initialize variables to store the results
cv_results <- data.frame(n_features = integer(), mean_rmse = numeric())

# Loop through different numbers of features
for (n_features in 1:length(importance)) {
  
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
  
  # Store the results
  cv_results <- rbind(cv_results, data.frame(n_features = n_features, mean_rmse = mean_rmse))
  
  print(paste0("Iteration number ",n_features,"/",length(importance)))
}

# Find the best number of features based on the lowest mean RMSE
best_n_features <- cv_results[which.min(cv_results$mean_rmse), "n_features"]
cat("Best number of features:", best_n_features)

# Select the best features names.
best_top_features <- names(importance)[order(importance, decreasing = TRUE)][1:best_n_features]

# Select only the variables from the best top features.
train_data_selected <- train_data[, c(best_top_features, "Rating")]

# 
final_rf_model <- ranger(Rating ~ ., data = train_data_selected)

test_data_selected <- test_data[, best_top_features]

predictions <- predict(final_rf_model, data = test_data_selected)$predictions


# SVM

control <- rfeControl(functions = caretFuncs, method = "cv", number = 10)

rfe_results <- rfe(train_data[, -which(names(train_data) == "Rating")],
                   train_data$Rating,
                   sizes = 1:ncol(train_data[, -which(names(train_data) == "Rating")]),
                   rfeControl = control)

best_n_features_svm <- rfe_results$optVariables
best_top_features_svm <- rfe_results$variables[1:best_n_features_svm, "varName"]

train_data_selected_svm <- train_data[, c(best_top_features_svm, "Rating")]
final_svm_model <- svm(Rating ~ ., data = train_data_selected_svm)
test_data_selected_svm <- test_data[, best_top_features_svm]
predictions_svm <- predict(final_svm_model, newdata = test_data_selected_svm)