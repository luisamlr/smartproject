################################################################
#### Smart Project: ETL - Data for Model Creation ####
################################################################

# Objective: to aggregate all the different data sources extracted from the step 
# before and make the neccesary transformations to create the models.

#### MAINTENANCE INSTRUCTIONS ####
#the last edit of this code took place on: (please update)
last_edit <- "2023-05-28"
### to-dos: ###
#Step 1. Running the code takes approximately 17 minutes
################################################################

#### Step 1: loading relevant libraries and importing data ####
## Install and load the required packages
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
library(tidyr)
library(readr)
library(geosphere)
library(httr)
library(jsonlite)
library(dplyr)

## Setting the working directory ##
setwd("C:/Users/radok/OneDrive/Desktop/Maastricht Univeristy/Service Project/Business Analytics/smartproject")
#setwd("~/Maastricht University/smartproject")

## Loading of the data sources ##
# All chargers locations, from Google Maps:
df_all <- read.csv("Step_1/All_Chargers.csv") # All Chargers, File loading
df_cs <- df_all[df_all$Ratings.Total>0,]
df_cs <- df_cs[df_cs$Country=="Netherlands" ,]

# Distance to the closes highway, from Google maps:
#highway_dist <- read.csv("highway_dist.csv") # Distance to the closes highway
#highway_dist <- select(highway_dist,charger_longitude,charger_latitude,highway) # Selecting only used columns

# Facilities around chargers, from OpenStreetMap:
poi_locations <- read.xlsx("facilities_around_coordinates_parking.xlsx") # File loading
poi_locations$type <- paste0("Fac_",poi_locations$type) # Add a prefix to improve handling
poi_locations$type <- make.names(poi_locations$type) # Fix type names to avoid problem on R
poi_locations$type <- ifelse(poi_locations$type %in% renaming$Old_names, 
                             renaming$New_names[match(poi_locations$type, renaming$Old_names)],
                             poi_locations$type) # Renaming POI types, fixing mislabeled names, and grouping similar named types.

# Reshaping the table
reshaped_poi_locations_pred <- poi_locations  %>%
  group_by(charger_longitude, charger_latitude, type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  spread(key = type, value = count, fill = 0)

# Adding postal code, city and country
potential_info <- read.csv("parking_spots_maastricht.csv") # File loading

# Demographic information for every Dutch postal code, From CBS Statistics Netherlands:
demog_data <- read_excel("CBS.xlsx") # File loading
names(demog_data) <- make.names(names(demog_data)) # Fix column names to avoid problem on R
demog_data_names <- colnames(demog_data) # Saving column names
indices <- match(demog_data_names, renaming$Old_names) # Find the indices of demog_data_names in renaming$Old_names
demog_data_names <- ifelse(!is.na(indices), renaming$New_names[indices], demog_data_names) # If there's a match, replace with renaming$New_names, else keep the original name
names(demog_data) <- demog_data_names # Assign the new column names to demog_data
demog_data <- demog_data[, !(colnames(demog_data) == "WijkenEnBuurten")] # Clean one column that is completely null
demog_data$StringValue <- as.numeric(demog_data$StringValue)

# Auxiliary file:
renaming <- read_excel("renaming.xlsx") # Vairable names maintenance


#### Step 2: Data cleaning ####
## COORDINATES DECIMAL CORRECTION ##
# Standardization of decimal coordinates in all files, to avoid problems when joining information
poi_locations$latitude <- round(as.numeric(poi_locations$latitude),7)
poi_locations$longitude <- round(as.numeric(poi_locations$longitude),7)
poi_locations$charger_latitude <- round(as.numeric(poi_locations$charger_latitude),7)
poi_locations$charger_longitude <- round(as.numeric(poi_locations$charger_longitude),7)
df_all$Latitude <- round(as.numeric(df_all$Latitude),7)
df_all$Longitude <- round(as.numeric(df_all$Longitude),7)
df_cs$Latitude <- round(as.numeric(df_cs$Latitude),7)
df_cs$Longitude <- round(as.numeric(df_cs$Longitude),7)
potential_info$latitude <- round(as.numeric(potential_info$latitude),7)
potential_info$longitude <- round(as.numeric(potential_info$longitude),7)

reshaped_poi_locations_pred <- left_join(reshaped_poi_locations_pred, select(potential_info, longitude, latitude, postal_code, city, country), by = c("charger_longitude" = "longitude", "charger_latitude" = "latitude"))
reshaped_poi_locations_pred <- reshaped_poi_locations_pred %>% filter(country == "Nederland")


#### Step 3: calculating average ratings ####
# Data cleaning #
# Select only those stations with review and inside the Netherlands
df_all<- df_all[df_all$Country=="Netherlands" ,]

# Function to calculate distance between two coordinates using geosphere package
calc_distance <- function(lat1, lon1, lat2, lon2) {
  dist <- distHaversine(c(lon1, lat1), c(lon2, lat2))
  return(dist)
}

# Function to find the nearest charging stations based on distance
find_nearest <- function(lat, lon, df_cs, closest = 1) {
  df_cs$Distance <- sapply(1:nrow(df_cs), function(i) {
    if (df_cs$Latitude[i] == lat && df_cs$Longitude[i] == lon) {
      return(Inf) # Set distance to infinity for charging station at given location
    } else {
      return(calc_distance(lat, lon, df_cs$Latitude[i], df_cs$Longitude[i]))
    }
  })
  df_sorted <- df_cs[order(df_cs$Distance),]
  return(head(df_sorted, n=closest))
}

# Function to compute average rating of the nearest charging stations
rating_nearest <- function(lat, lon, df, closest_cs = 1) {
  nearest_stations <- find_nearest(lat, lon, df,closest_cs)
  avg_rating <- mean(nearest_stations$Rating)
  return(avg_rating)
}

# initialize empty vector to store counts
counts_1000 <- c()
counts_500 <- c()
counts_250 <- c()
counts_100<-c()
# loop over each row in the dataframe

for (i in 1:nrow(reshaped_poi_locations_pred)) {
  # check if the row has a rating
  distances<-c()
  # calculate distance between this row and all other rows
  for (j in 1:nrow(df_all)){
    distances[j]<- distHaversine(c(reshaped_poi_locations_pred$charger_longitude[i], reshaped_poi_locations_pred$charger_latitude[i]), c(df_all$Longitude[j], df_all$Latitude[j]))
  }
  # count how many distances are less than 1000m
  count_1000 <- nrow(df_all[distances <= 1000,]) #Do not subtract 1 because in this case the potential station is not in the data frame
  # count how many distances are less than 500m
  count_500 <- nrow(df_all[distances <= 500,])  #Do not subtract 1 because in this case the potential station is not in the data frame
  # count how many distances are less than 250m
  count_250 <- nrow(df_all[distances <= 250,]) #Do not subtract 1 because in this case the potential station is not in the data frame
  # count how many distances are less than 100m
  count_100 <- nrow(df_all[distances <= 100,]) #Do not subtract 1 because in this case the potential station is not in the data frame
  # add count to vector
  counts_1000<- c(counts_1000, count_1000)
  # add count to vector
  counts_500<- c(counts_500, count_500)
  # add count to vector
  counts_250<- c(counts_250, count_250)
  # add count to vector
  counts_100<- c(counts_100, count_100)
  print(paste0("Calculating info for charger ", i," of ", nrow(reshaped_poi_locations_pred)))
}

# Select only Limburg stations to make code more efficient
df_limburg<-df_cs%>%
  filter(Admin.Area.Level.1 == "Limburg") 

n_rating <- c()
avg3 <- c()
avg5 <- c()
avg10 <- c()
for (i in 1:nrow(reshaped_poi_locations_pred)){
  # Find the nearest rating per CS
  n_rating[i] <- rating_nearest(reshaped_poi_locations_pred$charger_latitude[i],reshaped_poi_locations_pred$charger_longitude[i], df_limburg,1)
  # Find the avg 3 nearest rating per CS
  avg3[i] <- rating_nearest(reshaped_poi_locations_pred$charger_latitude[i], reshaped_poi_locations_pred$charger_longitude[i], df_limburg, 3)
  # Find the avg 5 nearest rating per CS
  avg5[i] <- rating_nearest(reshaped_poi_locations_pred$charger_latitude[i], reshaped_poi_locations_pred$charger_longitude[i], df_limburg, 5) 
  # Find the avg 10 nearest rating per CS
  avg10[i] <- rating_nearest(reshaped_poi_locations_pred$charger_latitude[i], reshaped_poi_locations_pred$charger_longitude[i], df_limburg, 10)
  print(paste0("Calculating info for charger ", i," of ", nrow(reshaped_poi_locations_pred)))
  }

#### Step 4: merging all of the above into one df ####
reshaped_poi_locations_pred <- cbind(reshaped_poi_locations_pred, n_rating, avg3,avg5, avg10, counts_100, counts_250, counts_500, counts_1000)

# Extract first 4 characters from the "postcode" column
reshaped_poi_locations_pred$postcode <- as.numeric(substr(reshaped_poi_locations_pred$postal_code, 1, 4))

# Identify non-numeric columns
non_numeric_columns <- c()
for (column in colnames(reshaped_poi_locations_pred)) {
  if (!is.numeric(reshaped_poi_locations_pred[[column]])) {
    non_numeric_columns <- c(non_numeric_columns, column)
  }
}

# Remove non-numeric columns
reshaped_poi_locations_pred <- reshaped_poi_locations_pred[, !(colnames(reshaped_poi_locations_pred) %in% non_numeric_columns)]

reshaped_poi_locations_pred <- reshaped_poi_locations_pred %>%
  left_join(demog_data, by = c("postcode" = "StringValue"))



#### Step 5: HIGHWAY DISTANCE ####
get_nearest_highway <- function(latitude, longitude) {
  overpass_api_url <- "https://overpass-api.de/api/interpreter"
  
  # List the highway types you are interested in (e.g., motorway, trunk, primary)
  highway_types <- c("motorway", "trunk", "primary")
  highway_filter <- paste0("highway~\"", paste(highway_types, collapse = "|"), "\"")
  
  overpass_query <- paste0(
    "[out:json];",
    "way[", highway_filter, "](around:1000,", latitude, ",", longitude, ");",
    "out geom;"
  )
  
  response <- POST(
    overpass_api_url,
    body = list(data = overpass_query),
    encode = "form"
  )
  
  if (response$status_code == 200) {
    content <- content(response, "text", encoding = "UTF-8")
    parsed <- fromJSON(content, flatten = TRUE)
    return(parsed)
  } else {
    warning("Request failed.")
    return(NULL)
  }
}

nearest_highway_distance <- function(latitude, longitude) {
  b <- c()
  highways <- get_nearest_highway(latitude, longitude)
  
  if (is.null(highways)) {
    return(NA)
  }
  min_distance <- Inf
  if (is.null(nrow(highways$elements))) {
    return(1001)
  } else{
    for (i in 1:nrow(highways$elements)) {
      way <- highways$elements[i,]
      
      for (j in 1:nrow(way$geometry[[1]])) {
        lat <- way$geometry[[1]]$lat[j]
        lon <- way$geometry[[1]]$lon[j]
        dist <- calc_distance(latitude, longitude, lat, lon)
        b <- c(b, dist)
        min_distance <- min(min_distance, dist)
      }
      print(paste0("Extracting highway info from OpenStreetMap's Map ",i ,"/",nrow(highways$elements)))
    }
  }
  return(min_distance)
}

highway_dist <- reshaped_poi_locations_pred[,1:2]

highway_dist <- highway_dist %>%
  rowwise() %>%
  mutate(highway_dist = nearest_highway_distance(charger_latitude, charger_longitude))
highway_dist$highway <- ifelse(highway_dist$highway_dist <=1000, 1, 0)

#### Step 6: Join together ####
reshaped_poi_locations_pred <- left_join(reshaped_poi_locations_pred, select(highway_dist, charger_longitude, charger_latitude, highway), by = c("charger_longitude", "charger_latitude"))


#### Step 7: Adjustments of dataframe ####
# Get column names of the two tables
columns_reshaped_poi_locations <- colnames(reshaped_poi_locations)
columns_reshaped_poi_locations_pred <- colnames(reshaped_poi_locations_pred)

# Find the columns that are only in reshaped_poi_location
extra_columns_reshaped_poi_locations <- setdiff(columns_reshaped_poi_locations, columns_reshaped_poi_locations_pred)

# Remove "Rating" column from the extra_columns list if it exists
extra_columns_reshaped_poi_locations <- extra_columns_reshaped_poi_locations[extra_columns_reshaped_poi_locations != "Rating"]

# Add the missing columns to reshaped_poi_location_pred and fill with 0
for (column in extra_columns_reshaped_poi_locations) {
  reshaped_poi_locations_pred[[column]] <- 0
}

# Find the columns that are only in reshaped_poi_location_pred
extra_columns_reshaped_poi_locations_pred <- setdiff(columns_reshaped_poi_locations_pred, columns_reshaped_poi_locations)

# Remove the extra columns from reshaped_poi_location_pred
reshaped_poi_locations_pred <- reshaped_poi_locations_pred[, !(colnames(reshaped_poi_locations_pred) %in% extra_columns_reshaped_poi_locations_pred)]

# Make sure the columns in reshaped_poi_location_pred are in the same order as in reshaped_poi_location
reshaped_poi_locations_pred <- reshaped_poi_locations_pred[, columns_reshaped_poi_locations[columns_reshaped_poi_locations %in% colnames(reshaped_poi_locations_pred)]]

# Save the data prepared for modeling creation
write_xlsx(reshaped_poi_locations_pred, "reshaped_poi_locations_potential.xlsx")











