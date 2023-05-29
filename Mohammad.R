## Mohammad

library(httr)
library(jsonlite)
library(googleway)
library(dplyr)
library(geosphere)
library(MLmetrics)
library(MASS)
library(caret)
library(xgboost)

key <- Sys.getenv("MY_API_KEY")

# Set the bounding box of Maastricht
# min_lat <- 50.7981
# max_lat <- 50.9030
# min_lon <- 5.6347
# max_lon <- 5.8309

min_lat <- 50.841423
max_lat <- 50.8581
min_lon <- 5.666937
max_lon <- 5.729

# Generate a grid of coordinates within the bounding box
n <- 20
latitudes <- seq(min_lat, max_lat, length.out = n)
longitudes <- seq(min_lon, max_lon, length.out = n)
coords <- expand.grid(latitudes, longitudes)


# Convert the grid to a data frame
locations <- data.frame(lat = coords[,1], lon = coords[,2])

radius <- 150
keyword <- "charging"

charging_points <- data.frame()
m <- c()

for (i in 1:nrow(locations)) {
  location <- paste0(locations[i,1], "%2C", locations[i,2])
  url <- paste0("https://maps.googleapis.com/maps/api/place/nearbysearch/json?",
                "location=", location,
                "&radius=", radius,
                "&keyword=", keyword,
                "&key=", key)
  response <- GET(url)
  places <- fromJSON(rawToChar(response$content))
  results <- places$results
  m <- c(m, nrow(results))
  if (!is.null(nrow(results))) {
    lat <- results$geometry$location$lat
    lng <- results$geometry$location$lng
    results <- cbind(lat, lng, results[, c("name", "place_id", "rating", "types", "user_ratings_total", "vicinity")])
    charging_points <- rbind(charging_points, results)
  }
}

all_centrum <- charging_points
all_centrum <- unique(all_centrum)


df <- highway_dist %>%
  right_join(reshaped_poi_locations, by = c("charger_latitude","charger_longitude"))
plot(df$Rating, df$highway_dist)

library(randomForest)
model1 <- lm(Rating ~ ., df)
summary(model1)
model2 <- randomForest(Rating ~ ., df)
plot(predict(model2,df), df$Rating)
summary(model2)


df <- reshaped_poi_locations
set.seed(123)

# Create the data partition for train and test sets
trainIndex <- createDataPartition(df$Rating, p = .7, 
                                  list = FALSE, 
                                  times = 1)

trainData <- df[ trainIndex,]
testData  <- df[-trainIndex,]

# Convert data to matrix format, which is required by xgboost
trainDataMatrix <- as.matrix(trainData[,-which(names(trainData) %in% "Rating")])
testDataMatrix <- as.matrix(testData[,-which(names(testData) %in% "Rating")])

# Set up the parameters for xgboost
params <- list(
  objective = "reg:squarederror", 
  eta = 0.2,
  max_depth = 2,
  min_child_weight = 1,
  subsample = 1,
  colsample_bytree = 1
)

# Convert the target variable into a vector
trainLabels <- as.vector(trainData$Rating)

# Train the model
xgb_model <- xgboost(data = trainDataMatrix, label = trainLabels, params = params, nrounds = 100)

# Predict on test data
predictions <- predict(xgb_model, testDataMatrix)

# You can check the performance of your model using RMSE or MAE
rmse <- sqrt(mean((testData$Rating - predictions)^2))
mae <- mean(abs(testData$Rating - predictions))

print(paste0("RMSE: ", rmse))
print(paste0("MAE: ", mae))

# Load necessary libraries
library(leaps)


# Define the response and predictor variables
df2 <- reshaped_poi_locations
columns_to_delete <- names(df2[3:714])[apply(df2, 2, function(x) max(x) < 2)]
df2 <- df2[, !(names(df2) %in% columns_to_delete)]

# c(559,561,563)
df <- df2[, c(177:370)]
response <- "Rating"
predictors <- names(df)[names(df) != response]

# Create a formula for the full model
options(expressions=10000)

formula <- as.formula(paste(response, paste(predictors, collapse = " + "), sep = " ~ "))

# Perform forward selection
result <- regsubsets(formula, data = df, method = "forward")

# Get the summary of the results
summary(result)
# dff <- na.omit(df)
# dff <- dff[!apply(is.infinite(dff), 1, any),]
# # Assuming 'mtcars' is your data frame
# dff <- dff[!rowSums(sapply(dff, is.infinite)), ]

# Get the summary of the results
summary_result <- summary(result)

# Find the model with the highest adjusted R-squared
best_model_size <- which.max(summary_result$adjr2)

# Get the names of the variables in the best model
best_model_vars <- names(coef(result, id = best_model_size))

# Create a new dataset with only the selected variables
new_data <- df[, best_model_vars[-1]]
df <- cbind(new_data, df$Rating)
colnames(df)[10] <- "Rating"



df <- reshaped_poi_locations[,1:2]



# Function to get POI count
get_poi_count <- function(lat, lon) {
  # Overpass API URL
  overpass_url <- "https://overpass-api.de/api/interpreter"
  
  # Initialize counts
  counts <- c("supermarket" = 0, "fitness_centre" = 0, "bar" = 0, "nightclub" = 0)
  
  # Loop over each point of interest type
  for (poi in names(counts)) {
    # Determine the correct key for the query
    key <- ifelse(poi == "supermarket", "shop", ifelse(poi == "fitness_centre", "leisure", "amenity"))
    
    # Construct the query
    query <- paste0(
      '[out:json];',
      'node["', key, '"="', poi, '"](around:1000,', lat, ',', lon, ');',
      'out count;'
    )
    
    # Send request
    response <- POST(
      overpass_url, 
      body = list(data = query), 
      encode = "form"
    )
    
    # Check status
    stop_for_status(response)
    
    # Parse response
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    # Extract count
    temp1 <- data$elements
    temp2 <- temp1$tags
    counts[poi] <- temp2$total
  }
  
  return(counts)
}


# Assuming your dataframe is named df and the first two columns are latitude and longitude

# Apply the function to the dataframe
poi_counts <- t(apply(df[, 1:2], 1, function(x) get_poi_count(x[2], x[1])))

# Add the counts to the dataframe
df$supermarkets <- poi_counts[, "supermarket"]
df$gyms <- poi_counts[, "fitness_centre"]
df$bars <- poi_counts[, "bar"]
df$clubs <- poi_counts[, "nightclub"]


poi <- df
df <- cbind(poi, reshaped_poi_locations[,715:783])
# Assuming your dataframe is named 'df'
columns_to_convert <- 3:6
df[, columns_to_convert] <- lapply(df[, columns_to_convert], as.numeric)

# Assuming your dataframe is named 'df'
# Assuming 'Rating' is a numeric column

# Calculate the average rating
average_rating <- mean(df$Rating, na.rm = TRUE)

# Replace values higher than average with 1, and values below average with 0
df$Rating <- ifelse(df$Rating > average_rating, 1, 0)
df$Rating <- as.factor(df$Rating)

# Assuming your dataframe is named 'df'
# Assuming 'Rating' is the target column and other columns are predictors

# Set seed for reproducibility
set.seed(123)

# Split the data into train and test sets
train_indices <- sample(1:nrow(df), nrow(df)*0.7)  # 70% for training
train_set <- df[train_indices, ]
test_set <- df[-train_indices, ]

# Load required packages
library(e1071)

# Train the SVM model
model <- randomForest(Rating ~ ., data = train_set)

# Make predictions on the test set
predictions <- predict(model, newdata = test_set)
predictions <- ifelse(predictions >= 0.5, 1, 0)

# Calculate accuracy
accuracy <- sum(predictions == test_set$Rating) / nrow(test_set)

# Create a confusion matrix
confusion_matrix <- table(Actual = test_set$Rating, Predicted = predictions)

# Print the accuracy and confusion matrix
cat("Accuracy:", accuracy, "\n")
cat("\nConfusion Matrix:\n")
print(confusion_matrix)

