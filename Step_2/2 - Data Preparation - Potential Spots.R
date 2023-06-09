################################################################
#### Smart Project: ETL - Data for Model Creation ####
################################################################

# Objective: to aggregate all the different data sources extracted from the step 
# before and make the necessary transformations to create the models.

#### MAINTENANCE INSTRUCTIONS ####
#the last edit of this code took place on: (please update)
last_edit <- "2023-05-28"
### to-dos: ###
#S1. Running the code takes approximately 17 minutes
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

# Set the working directory to the location of the script.
# On RStudio, the code below can be used. If not, please specify manually the folder where this script is located.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("/path/to/your/script")

## Loading of the data sources ##
df_all <- read.csv("source_data/All_Chargers.csv") # Load chargers' data from CSV.
df_cs <- df_all[df_all$Ratings.Total>0,]      # Filter chargers with reviews.
df_cs <- df_cs[df_cs$Country=="Netherlands" ,] # Retain only Netherlands-based chargers.

# Auxiliary file:
renaming <- read_excel("source_data/variable_maintenance.xlsx")

# Distance to the closes highway, from Google maps:
highway_dist <- read.csv("source_data/highway_dist.csv") # Distance to the closes highway
highway_dist <- select(highway_dist,charger_longitude,charger_latitude,highway) # Selecting only used columns

# Facilities around chargers, from OpenStreetMap:
poi_locations <- read.csv("source_data/facilities_around_coordinates_parking.csv") # File loading
poi_locations$type <- paste0("Fac_",poi_locations$type) # Add a prefix to improve handling.
poi_locations$type <- make.names(poi_locations$type) # Fix type names to avoid problem on R.

# Reshaped Poi Location from the model creation file
reshaped_poi_locations <- read_excel("output/reshaped_poi_locations.xlsx")

# Step 1: Remove rows where 'type' from 'poi_locations' is not in 'Old.Names' of 'renaming'
poi_locations <- poi_locations[poi_locations$type %in% renaming$Old_names,]
names_to_include <- renaming$Old_names[renaming$Include_Variable == 1] # Get the names that should be included
poi_locations <- poi_locations[poi_locations$type %in% names_to_include, ] # Filter the poi_locations to only include those names
poi_locations$type <- ifelse(poi_locations$type %in% renaming$Old_names,
                             ifelse(!is.na(renaming$FAC_Aggregation[match(poi_locations$type, renaming$Old_names)]) & nchar(renaming$FAC_Aggregation[match(poi_locations$type, renaming$Old_names)]) > 0,
                                    renaming$FAC_Aggregation[match(poi_locations$type, renaming$Old_names)],
                                    renaming$FAC_Aggregation[match(poi_locations$type, renaming$Old_names)]),
                             poi_locations$type) # Renaming POI types, fixing mislabeled names, and grouping similar named types.

# Reshaping the table
reshaped_poi_locations_pred <- poi_locations  %>%
  group_by(charger_longitude, charger_latitude, type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  spread(key = type, value = count, fill = 0)

# Adding postal code, city and country
potential_info <- read.csv("source_data/parking_spots_maastricht.csv") # File loading

# Demographic information for every Dutch postal code, From CBS Statistics Netherlands:
demog_data <- read_excel("source_data/CBS.xlsx") # File loading
names(demog_data) <- make.names(names(demog_data)) # Fix column names to avoid problem on R.
demog_data_names <- colnames(demog_data) # Saving column names.
indices <- match(demog_data_names, renaming$Old_names) # Find the indices of demog_data_names in renaming$Old_names
demog_data_names <- ifelse(!is.na(indices), renaming$New_names[indices], demog_data_names) # If there's a match, replace with renaming$New_names, else keep the original name
names(demog_data) <- demog_data_names # Assign the new column names to demog_data
names_to_exclude <- renaming$Old_names[renaming$Include_Variable == 0] # Get the column names that should be excluded
demog_data <- demog_data[ , !(names(demog_data) %in% names_to_exclude)] # Drop the columns from demog_data

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

#for (i in 1:nrow(reshaped_poi_locations_pred)) {
#  # check if the row has a rating
#  distances<-c()
#  # calculate distance between this row and all other rows
#  for (j in 1:nrow(df_all)){
#    distances[j]<- distHaversine(c(reshaped_poi_locations_pred$charger_longitude[i], reshaped_poi_locations_pred$charger_latitude[i]), c(df_all$Longitude[j], df_all$Latitude[j]))
#  }
#  # count how many distances are less than 1000m
#  count_1000 <- nrow(df_all[distances <= 1000,]) #Do not subtract 1 because in this case the potential station is not in the data frame
#  # count how many distances are less than 500m
#  count_500 <- nrow(df_all[distances <= 500,])  #Do not subtract 1 because in this case the potential station is not in the data frame
#  # count how many distances are less than 250m
#  count_250 <- nrow(df_all[distances <= 250,]) #Do not subtract 1 because in this case the potential station is not in the data frame
#  # count how many distances are less than 100m
#  count_100 <- nrow(df_all[distances <= 100,]) #Do not subtract 1 because in this case the potential station is not in the data frame
#  # add count to vector
#  counts_1000<- c(counts_1000, count_1000)
#  # add count to vector
#  counts_500<- c(counts_500, count_500)
#  # add count to vector
#  counts_250<- c(counts_250, count_250)
#  # add count to vector
#  counts_100<- c(counts_100, count_100)
#  print(paste0("Calculating info for charger ", i," of ", nrow(reshaped_poi_locations_pred)))
#}
# write.csv(counts_1000, "source_data/p_counts_1000.csv", row.names = FALSE)
# write.csv(counts_500, "source_data/p_counts_500.csv", row.names = FALSE)
# write.csv(counts_250, "source_data/p_counts_250.csv", row.names = FALSE)
# write.csv(counts_100, "source_data/p_counts_100.csv", row.names = FALSE)

# If the above loop is commented out, use this output:
counts_1000_csv <- read.csv("source_data/p_counts_1000.csv"); counts_1000 <- as.numeric(counts_1000_csv[[1]])
counts_500_csv <- read.csv("source_data/p_counts_500.csv"); counts_500 <- as.numeric(counts_500_csv[[1]])
counts_250_csv <- read.csv("source_data/p_counts_250.csv"); counts_250 <- as.numeric(counts_250_csv[[1]])
counts_100_csv <- read.csv("source_data/p_counts_100.csv"); counts_100 <- as.numeric(counts_100_csv[[1]])


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
write_xlsx(reshaped_poi_locations_pred, "output/reshaped_poi_locations_potential.xlsx")