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
library(tidyr)

setwd("C:/Users/radok/OneDrive/Desktop/Maastricht Univeristy/Service Project/Business Analytics/All Files/") 

### Loading the extracted files obtained from the Google API: ###

# Listing all the files present in the specified directory.
file_list <- list.files()

# Read all the files and bind them into a single data frame.
all_data <- lapply(file_list, function(file) {
  read_delim(file, col_types = cols(.default = "c"), delim = ';')  # read_delim() to specify the delimiter
}) %>% bind_rows()

# Eliminate duplicates based on all columns.
unique_data <- all_data %>%
  distinct()

# The consolidated file is saved as a CSV for manual checking and analysis of the data.
write_csv(unique_data, "C:/Users/radok/OneDrive/Desktop/Maastricht Univeristy/Service Project/Business Analytics/Consolidated Files/All_Chargers.csv")

# Creating an additional file, only containing the rows where ratings are present.
unique_data_rv <- unique_data %>%
  filter(`Ratings Total` > 0)

write_csv(unique_data_rv, "C:/Users/radok/OneDrive/Desktop/Maastricht Univeristy/Service Project/Business Analytics/Consolidated Files/ev_stations_review.csv")

### Consolidation of Data from Diverse Sources ###

# Loading of the data.
ev_chargers_reviews <- read.csv("All_Chargers.csv") # All chargers location, from GMaps.
poi_locations <- read_excel("facilities_around_coordinates.xlsx") # Facilities around chargers, from OpenStreetMap
demog_data <- read_excel("CBS.xlsx") # Demographic information for every Dutch postal code, From CBS
parking_maastricht <- read_excel("parking_spots_maastricht.xlsx") # Maastrich parking spots, from Gmaps.
aggregate_rating <- read_excel("data_variables_roger.xlsx") # Aggregate information of every charger.
aggregate_rating <- select(aggregate_rating, Latitude, Longitude, n_rating, avg3, avg5, avg10, counts_100, counts_250, counts_500, counts_1000)
highway_dist <- read.csv("highway_dist.csv") # Distance to the closes highway, from Gmaps.
highway_dist <- select(highway_dist,charger_longitude,charger_latitude)

# Eliminate duplicates.
ev_chargers_reviews <- distinct(ev_chargers_reviews)
poi_locations <- distinct(poi_locations)
demog_data <- distinct(demog_data)
parking_maastricht <- distinct(parking_maastricht)
aggregate_rating <- distinct(aggregate_rating)

# Standardization of decimal coordinates in all files, to avoid problems when joining information.
poi_locations$latitude <- round(as.numeric(poi_locations$latitude),7)
poi_locations$longitude <- round(as.numeric(poi_locations$longitude),7)
poi_locations$charger_latitude <- round(as.numeric(poi_locations$charger_latitude),7)
poi_locations$charger_longitude <- round(as.numeric(poi_locations$charger_longitude),7)
ev_chargers_reviews$Latitude <- round(as.numeric(ev_chargers_reviews$Latitude),7)
ev_chargers_reviews$Longitude <- round(as.numeric(ev_chargers_reviews$Longitude),7)
parking_maastricht$latitude <- round(as.numeric(parking_maastricht$latitude),7)
parking_maastricht$longitude <- round(as.numeric(parking_maastricht$longitude),7)
aggregate_rating$Latitude <- round(as.numeric(aggregate_rating$Latitude),7)
aggregate_rating$Longitude <- round(as.numeric(aggregate_rating$Longitude),7)

# Additional data format corrections.
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
  R <- 6371 # Earth's mean radius in kilometers
  d <- R * c
  
  return(d)
}

# Applying Haversine distance
poi_locations$distance <- mapply(haversine_distance, poi_locations$charger_latitude, poi_locations$charger_longitude, poi_locations$latitude, poi_locations$longitude)
poi_locations$distance <- as.numeric(poi_locations$distance)

# Ensure that the key columns in both data frames have the same names
names(ev_chargers_reviews)[names(ev_chargers_reviews) == "Longitude"] <- "charger_longitude"
names(ev_chargers_reviews)[names(ev_chargers_reviews) == "Latitude"] <- "charger_latitude"

names(demog_data)[names(demog_data) == "Longitude"] <- "charger_longitude"
names(demog_data)[names(demog_data) == "Latitude"] <- "charger_latitude"

names(aggregate_rating)[names(aggregate_rating) == "Longitude"] <- "charger_longitude"
names(aggregate_rating)[names(aggregate_rating) == "Latitude"] <- "charger_latitude"

# Reshaping the table
reshaped_poi_locations <- poi_locations %>%
  group_by(charger_longitude, charger_latitude, type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  spread(key = type, value = count, fill = 0)

# Adding charger information
reshaped_poi_locations <- reshaped_poi_locations %>%
  left_join(ev_chargers_reviews, by = c("charger_longitude" = "charger_longitude", "charger_latitude" = "charger_latitude"))

# Trimed postal code for cbs merge
reshaped_poi_locations$postal_trim <- substr(reshaped_poi_locations$`Postal.Code`, start = 1, stop = 4)

# Left join the demog_data
reshaped_poi_locations <- reshaped_poi_locations %>%
  left_join(demog_data, by = c("postal_trim"="StringValue"))

# Adding aggregate charger information
reshaped_poi_locations <- reshaped_poi_locations %>%
  left_join(aggregate_rating, by = c("charger_longitude" = "charger_longitude", "charger_latitude" = "charger_latitude"))

# Clean one column that is completely null
reshaped_poi_locations <- reshaped_poi_locations[, !(colnames(reshaped_poi_locations) == "WijkenEnBuurten")]

# Checking and fixing variable names
names(reshaped_poi_locations) <- make.names(names(reshaped_poi_locations))
reshaped_poi_locations <- na.omit(reshaped_poi_locations)

# Some variables needs to be deleted for the model (e.g Postal Code), so this step generate an excel to select those variables.
# Convert the column names to a single column using gather()
col_names_review <- gather(data.frame(names(reshaped_poi_locations)), "Model Variables")

# Write the column names to an Excel file using writexl
write_xlsx(col_names_review, "column_names.xlsx")

# Extract variables before running the models
reshaped_poi_locations <- select(reshaped_poi_locations, -Name, -Address, -Reviews, -Ratings.Total, -Postal.Code, -Street.Number, -Route, -Locality, -Admin.Area.Level.1, -Admin.Area.Level.2, -Country, -Phone.Number, -Website, -Opening.Hours, -postal_trim)

# Save the data prepared for modeling creation
write_xlsx(reshaped_poi_locations, "reshaped_poi_locations.xlsx")

