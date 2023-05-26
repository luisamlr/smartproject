
library(tidyverse)
library(readr)


# Set the working directory.
setwd("C:/Users/radok/OneDrive/Desktop/Maastricht Univeristy/Service Project/Business Analytics/All Files/")
#setwd("~/Maastricht University/smartproject")
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
write_csv(unique_data, "All_Chargers.csv")

# Creating an additional file, only containing the rows where ratings are present.
unique_data_rv <- unique_data %>%
  filter(`Ratings Total` > 0)

write_csv(unique_data_rv, "ev_stations_review.csv")