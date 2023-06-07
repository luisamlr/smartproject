#########################################
#### Smart Project: Nearest Highways ####
#########################################

#This R script uses the Overpass API to find the nearest highway to each point of interest specified by latitude and longitude. 
#It calculates the distance from each point of interest to the nearest highway, and indicates whether each point is within 1000 meters of a highway. 
#It writes the results to a CSV file.

# Set the working directory to the location of the script.
# On RStudio, the code below can be used. If not, please specify manually the folder where this script is located.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("/path/to/your/script")

# Import necessary libraries
library(httr)
library(jsonlite)
library(dplyr)
library(readr)

# Load the data
All_Chargers <- read_csv("output/All_Chargers.csv")
parking_spots_maastricht <- read_csv("output/parking_spots_maastricht.csv")

# Extract latitude and longitude where Ratings Total > 0, eliminate duplicates
All_Chargers_unique <- All_Chargers %>%
  filter(`Ratings.Total` > 0) %>%
  select(Latitude, Longitude) %>%
  distinct()

# Rename columns
colnames(All_Chargers_unique) <- c("charger_latitude", "charger_longitude")

# Add new column to flag origin
All_Chargers_unique <- mutate(All_Chargers_unique, origin = "Chargers")

# Extract charger_latitude and charger_longitude, eliminate duplicates
facilities_unique <- parking_spots_maastricht %>%
  select(latitude, longitude) %>%
  distinct()

# Rename columns
colnames(facilities_unique) <- c("charger_latitude", "charger_longitude")

# Add new column to flag origin
facilities_unique <- mutate(facilities_unique, origin = "Potential")

# Combine the two tables
combined_table <- bind_rows(All_Chargers_unique, facilities_unique)

### Calculating if there is a nearest highway ###

# Define a function that queries the Overpass API to find the nearest highway given a latitude and longitude
get_nearest_highway <- function(latitude, longitude) {
  # Define the URL for the Overpass API
  overpass_api_url <- "https://overpass-api.de/api/interpreter"
  
  # Define the types of highways you are interested in
  highway_types <- c("motorway", "trunk", "primary")
  
  # Create a filter for the Overpass query to only return highways of the types you're interested in
  highway_filter <- paste0("highway~\"", paste(highway_types, collapse = "|"), "\"")
  
  # Create the Overpass query string
  overpass_query <- paste0(
    "[out:json];",
    "way[", highway_filter, "](around:1000,", latitude, ",", longitude, ");",
    "out geom;"
  )
  
  # Make a POST request to the Overpass API with the query string
  response <- POST(
    overpass_api_url,
    body = list(data = overpass_query),
    encode = "form"
  )
  
  # If the request is successful (status code 200), parse the response as JSON and return the result
  if (response$status_code == 200) {
    content <- content(response, "text", encoding = "UTF-8")
    parsed <- fromJSON(content, flatten = TRUE)
    return(parsed)
  } else {
    # If the request is not successful, issue a warning and return NULL
    warning("Request failed.")
    return(NULL)
  }
}

# Define a function to calculate the distance (in meters) between two points specified by latitude and longitude
distance <- function(lat1, lon1, lat2, lon2) {
  # Constants
  r <- 6371000 # Earth's radius in meters
  
  # Convert latitudes and longitudes from degrees to radians
  phi1 <- lat1 * pi / 180
  phi2 <- lat2 * pi / 180
  delta_phi <- (lat2 - lat1) * pi / 180
  delta_lambda <- (lon2 - lon1) * pi / 180
  
  # Calculate the distance using the Haversine formula
  a <- sin(delta_phi / 2)^2 + cos(phi1) * cos(phi2) * sin(delta_lambda / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  return(r * c)
}

# Define a function that calculates the distance to the nearest highway for a given point specified by latitude and longitude
nearest_highway_distance <- function(latitude, longitude) {
  b <- c()
  
  # Get the nearest highways to the point
  highways <- get_nearest_highway(latitude, longitude)
  
  # If no highways are found, return NA
  if (is.null(highways)) {
    return(NA)
  }
  
  # Initialize the minimum distance to Inf (infinity)
  min_distance <- Inf
  
  # If there are no elements in the highways data, return 1001
  if (is.null(nrow(highways$elements))) {
    return(1001)
  } else {
    # For each highway, calculate the distance to each point along the highway and keep track of the minimum distance
    for (i in 1:nrow(highways$elements)) {
      way <- highways$elements[i,]
      
      for (j in 1:nrow(way$geometry[[1]])) {
        lat <- way$geometry[[1]]$lat[j]
        lon <- way$geometry[[1]]$lon[j]
        dist <- distance(latitude, longitude, lat, lon)
        b <- c(b, dist)
        min_distance <- min(min_distance, dist)
      }
    }
  }
  
  # Return the minimum distance
  return(min_distance)
}

### Calculations for the nearest highway around EV chargers ###

# Define a data frame containing only the latitude and longitude of each point of interest
highway_dist <- combined_table

# Calculate the distance to the nearest highway for each point of interest
highway_dist <- highway_dist %>%
  rowwise() %>%
  mutate(highway_dist = nearest_highway_distance(charger_latitude, charger_longitude))

# Create a binary variable indicating whether each point of interest is within 1000 meters of a highway
highway_dist$highway <- ifelse(highway_dist$highway_dist <=1000, 1, 0)

# Write the data frame to a CSV file
write.csv(highway_dist, "output/highway_dist.csv")
