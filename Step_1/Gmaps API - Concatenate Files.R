# This code

# Load the required packages
library(readr)
library(dplyr)
library(tmap)
library(sp)

setwd("C:/Users/radok/OneDrive/Desktop/Maastricht Univeristy/Service Project/Business Analytics/All Files/") 



# List all the files that start with "charging_stations_gmaps"
file_list <- list.files()

# Read all the files and bind them into a single data frame
all_data <- lapply(file_list, function(file) {
  read_delim(file, col_types = cols(.default = "c"), delim = ';')  # Use read_delim() to specify the delimiter
}) %>% bind_rows()

# Eliminate duplicates based on all columns
unique_data <- all_data %>%
  distinct()

# If you want to eliminate duplicates based on specific columns, use the following code instead
# unique_data <- all_data %>%
#   distinct(Name, Latitude, Longitude, Address, .keep_all = TRUE)

# Save the unique data to a new CSV file
write_csv(unique_data, "C:/Users/radok/OneDrive/Desktop/Maastricht Univeristy/Service Project/Business Analytics/Consolidated Files/All_Chargers.csv")

unique_data_rv <- unique_data %>%
  filter(`Ratings Total` > 0)

write_csv(unique_data_rv, "C:/Users/radok/OneDrive/Desktop/Maastricht Univeristy/Service Project/Business Analytics/Consolidated Files/ev_stations_review.csv")



#df <- unique_data%>%
#  group_by(Locality)%>%
#  summarise(obs=n())

# List all the files that start with "locations"
file_list_locations <- list.files(pattern = "^locations")

# Read all the files and bind them into a single data frame
all_locations <- lapply(file_list_locations, function(file) {
  read_delim(file, col_types = cols(.default = "c"), delim = ';')  # Use read_delim() to specify the delimiter
}) %>% bind_rows()

# Save the combined locations data to a new CSV file
write_csv(all_locations, "all_locations.csv")

ev_stations <- unique_data %>%
  select(Name, Latitude, Longitude)

write_csv(ev_stations, "all_evstations.csv")
