################################################################
#### Smart Project: Data Merge, Cleaning & Variable Control ####
################################################################

# Objective: Consolidation of Data from Diverse Sources


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

## Setting the working directory ##
#setwd("C:/Users/radok/OneDrive/Desktop/Maastricht Univeristy/Service Project/Business Analytics/smartproject") #setwd("~/Maastricht University/smartproject")
#setwd("~/Maastricht University/smartproject")
setwd("/Users/isamahler/Desktop/SmartRProject/newsmart/Step_1")


## Loading of the data sources ##
# All chargers locations, from Google Maps:
df_cs <- read.csv("All_Chargers.csv")
# Facilities around chargers, from OpenStreetMap:
poi_locations <- read_excel("facilities_around_coordinates.xlsx")
# Demographic information for every Dutch postal code, From CBS:
demog_data <- read_excel("CBS.xlsx")
# Maastricht parking spots, from Google maps:
parking_maastricht <- read_excel("parking_spots_maastricht.xlsx")
# Distance to the closes highway, from Google maps:
highway_dist <- read.csv("highway_dist.csv")
# Auxiliary file:
renaming <- read_excel("renaming.xlsx")


#### Step 2: Data cleaning ####

## GOOGLE MAPS ##
# Selecting only used columns
highway_dist <- select(highway_dist,charger_longitude,charger_latitude,highway) # selecting only used columns
# Selecting only those stations with review and inside the Netherlands
df_cs <- df_cs[df_cs$Ratings.Total>0,]
df_cs <- df_cs[df_cs$Country=="Netherlands" ,]
# Replace "North" with "Noord" and " " with "-" in the Admin.Area.Level.1 column
df_cs$Admin.Area.Level.1 <- gsub("North", "Noord", df_cs$Admin.Area.Level.1)
df_cs$Admin.Area.Level.1 <- gsub("South", "Zuid", df_cs$Admin.Area.Level.1)
df_cs$Admin.Area.Level.1 <- gsub(" ", "-", df_cs$Admin.Area.Level.1)
# unique(df_cs$Admin.Area.Level.1) # 12 provinces in Netherlands


## OPEN STREET MAP ##
poi_locations$type <- paste0("Fac_",poi_locations$type) # Add a prefix to improve handling.
poi_locations$type <- make.names(poi_locations$type) # Fix type names to avoid problem on R.
poi_locations$type <- ifelse(poi_locations$type %in% renaming$Old_names, 
                   renaming$New_names[match(poi_locations$type, renaming$Old_names)],
                   poi_locations$type) # Renaming POI types, fixing mislabeled names, and grouping similar named types.

## CBS STATISTICS NETHERLANDS ##
names(demog_data) <- make.names(names(demog_data)) # Fix column names to avoid problem on R.
demog_data_names <- colnames(demog_data) # Saving column names.
indices <- match(demog_data_names, renaming$Old_names) # Find the indices of demog_data_names in renaming$Old_names
demog_data_names <- ifelse(!is.na(indices), renaming$New_names[indices], demog_data_names) # If there's a match, replace with renaming$New_names, else keep the original name
names(demog_data) <- demog_data_names # Assign the new column names to demog_data
demog_data <- demog_data[, !(colnames(demog_data) == "WijkenEnBuurten")] # Clean one column that is completely null.


#### Step 3: calculating average ratings ####
### Finding the averages of the 3, 5 or 10 nearest charging stations

# Function to calculate distance between two coordinates using geosphere package
calc_distance <- function(lat1, lon1, lat2, lon2) {
  dist <- distHaversine(c(lon1, lat1), c(lon2, lat2))
  return(dist)
}

# Function to find the nearest charging stations based on distance
find_nearest <- function(lat, lon, df_cs, closest = 1) {
  df_cs$Distance <- sapply(1:nrow(df_cs), function(i) {
    if (df_cs$Latitude[i] == lat && df_cs$Longitude[i] == lon) {
      return(Inf) # setting distance to infinity for charging station at given location
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

# Starting empty dataframes for using in the next calculation.
n_rating <- c()
avg3 <- c()
avg5 <- c()
avg10 <- c()

for (i in 1:nrow(df_cs)){
  # Find the nearest rating per CS
  n_rating[i]<-rating_nearest(df_cs$Latitude[i], df_cs$Longitude[i], df_cs,1)
  # Find the avg 3 nearest rating per CS
  avg3[i]<-rating_nearest(df_cs$Latitude[i], df_cs$Longitude[i], df_cs, 3)
  # Find the avg 5 nearest rating per CS
  avg5[i]<-rating_nearest(df_cs$Latitude[i], df_cs$Longitude[i], df_cs, 5)
  # Find the avg 10 nearest rating per CS
  avg10[i]<-rating_nearest(df_cs$Latitude[i], df_cs$Longitude[i], df_cs, 10)
  print(paste0("Calculating info for charger ", i," of ", nrow(df_cs)))
}


# Finding the closest charging stations, save the distance
closest <- function(lat, lon, df_cs) {
  df_cs$Distance <- sapply(1:nrow(df_cs), function(i) calc_distance(lat, lon, df_cs$Latitude[i], df_cs$Longitude[i]))
  df_sorted <- df_cs[order(df_cs$Distance),]
  return(head(df_sorted$Distance, n=1))
}

cls<-c()
for (i in 1:nrow(df_cs)){
  # avg_rating_3_nearest function for Station1
  cls[i]<-closest(df_cs$Latitude[i], df_cs$Longitude[i], df_cs)
  print(paste0("Calculating info for charger ", i," of ", nrow(df_cs)))
}

## Calculating how many stations are under 1000m, 500m and 200m
# Import the dataset ---- We work with the whole dataset since we do not care whether they have review or not in order to take into account
df_all<-read.csv("All_Chargers.csv")
df_all<-df_all[df_all$Country=="Netherlands" ,]

# initialize empty vector to store counts
counts_1000 <- c()
counts_500 <- c()
counts_250 <- c()
counts_100<-c()
# loop over each row in the dataframe

for (i in 1:nrow(df_all)) {
  # check if the row has a rating
  if ((df_all$Ratings.Total[i] > 0)) {
    # check if the row has a rating
    distances<-c()
    # calculate distance between this row and all other rows
    for (j in 1:nrow(df_all)){
      distances[j]<- distHaversine(c(df_all$Longitude[i], df_all$Latitude[i]), c(df_all$Longitude[j], df_all$Latitude[j]))
    }
    # count how many distances are less than 1000m
    count_1000 <- nrow(df_all[distances <= 1000,]) - 1 #Subtract 1 because otherwise we count the station itself
    # count how many distances are less than 500m
    count_500 <- nrow(df_all[distances <= 500,])  - 1 #Subtract 1 because otherwise we count the station itself
    # count how many distances are less than 250m
    count_250 <- nrow(df_all[distances <= 250,])  - 1 #Subtract 1 because otherwise we count the station itself
    # count how many distances are less than 100m
    count_100 <- nrow(df_all[distances <= 100,])  - 1 #Subtract 1 because otherwise we count the station itself
    # add count to vector
    counts_1000<- c(counts_1000, count_1000)
    # add count to vector
    counts_500<- c(counts_500, count_500)
    # add count to vector
    counts_250<- c(counts_250, count_250)
    # add count to vector
    counts_100<- c(counts_100, count_100)
  }
  print(paste0("Calculating info for charger ", i," of ", nrow(df_all)))
}

# Merging with previous information
aggregate_rating<-cbind(df_cs, n_rating, avg3,avg5, avg10, counts_100, counts_250, counts_500, counts_1000)

# Eliminating duplicates
# ev_chargers_reviews <- distinct(ev_chargers_reviews)
poi_locations <- distinct(poi_locations)
demog_data <- distinct(demog_data)
parking_maastricht <- distinct(parking_maastricht)
aggregate_rating <- distinct(aggregate_rating)

# Standardization of decimal coordinates in all files, to avoid problems when joining information.
poi_locations$latitude <- round(as.numeric(poi_locations$latitude),7)
poi_locations$longitude <- round(as.numeric(poi_locations$longitude),7)
poi_locations$charger_latitude <- round(as.numeric(poi_locations$charger_latitude),7)
poi_locations$charger_longitude <- round(as.numeric(poi_locations$charger_longitude),7)
parking_maastricht$latitude <- round(as.numeric(parking_maastricht$latitude),7)
parking_maastricht$longitude <- round(as.numeric(parking_maastricht$longitude),7)
aggregate_rating$Latitude <- round(as.numeric(aggregate_rating$Latitude),7)
aggregate_rating$Longitude <- round(as.numeric(aggregate_rating$Longitude),7)

# Additional data format corrections
demog_data$StringValue <- as.character(demog_data$StringValue)

# Calculating the distance between chargers and points of interest.
poi_locations$distance <- mapply(calc_distance, poi_locations$charger_latitude, poi_locations$charger_longitude, poi_locations$latitude, poi_locations$longitude)
poi_locations$distance <- as.numeric(poi_locations$distance)

# Ensuring that the key columns in both data frames have the same names.
names(demog_data)[names(demog_data) == "Longitude"] <- "charger_longitude"
names(demog_data)[names(demog_data) == "Latitude"] <- "charger_latitude"
names(aggregate_rating)[names(aggregate_rating) == "Longitude"] <- "charger_longitude"
names(aggregate_rating)[names(aggregate_rating) == "Latitude"] <- "charger_latitude"

# Counting the number of facilities around each charger, and reshaping this information horizontally (1 row = 1 charger).
reshaped_poi_locations <- poi_locations %>%
  group_by(charger_longitude, charger_latitude, type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  spread(key = type, value = count, fill = 0)

# Adding charger information from Google Maps (reviews, ratings and geographical information)
reshaped_poi_locations <- reshaped_poi_locations %>%
  left_join(aggregate_rating, by = c("charger_longitude" = "charger_longitude", "charger_latitude" = "charger_latitude"))

# Creating a trimmed postal code with only the first four characters to be compatible with the base from CBS.
reshaped_poi_locations$postal_trim <- substr(reshaped_poi_locations$`Postal.Code`, start = 1, stop = 4)

# Left join the demog_data from CSB.
reshaped_poi_locations <- reshaped_poi_locations %>%
  left_join(demog_data, by = c("postal_trim"="StringValue"))

# Left join the highway_data from Gmaps.
reshaped_poi_locations <- reshaped_poi_locations %>%
  left_join(highway_dist, by = c("charger_longitude" = "charger_longitude", "charger_latitude" = "charger_latitude"))

# Fixing variable names, to avoid problem with columns names starting with numbers. Cleaning rows with NAs.
names(reshaped_poi_locations) <- make.names(names(reshaped_poi_locations))

reshaped_poi_locations <- na.omit(reshaped_poi_locations)

# Before saving the file to be use in model creations steps, is neccesary to remove an
reshaped_poi_locations <- select(reshaped_poi_locations, -Name, -Address, -Reviews, -Ratings.Total, -Postal.Code, -Street.Number, -Route, -Locality, -Admin.Area.Level.1, -Admin.Area.Level.2, -Country, -Phone.Number, -Website, -Opening.Hours, -postal_trim)

# Save the data prepared for modeling creation
write_xlsx(reshaped_poi_locations, "reshaped_poi_locations.xlsx")
