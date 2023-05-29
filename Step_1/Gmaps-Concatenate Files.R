#######################################################
#### Smart Project: API Files Load and Preparation ####
#######################################################


# Import necessary libraries
library(tidyverse)
library(readr)

# Set the working directory where the files are located
setwd("C:/Users/radok/OneDrive/Desktop/Maastricht Univeristy/Service Project/Business Analytics/All Files/")

# List all files in the current directory
file_list <- list.files()

# Read each file in the list of files and append them into one data frame
# read_delim() is used with a semi-colon (;) delimiter to read the file
all_data <- lapply(file_list, function(file) {
  read_delim(file, col_types = cols(.default = "c"), delim = ';')  
}) %>% bind_rows() # bind_rows() combines all data frames in the list into a single data frame

# Remove duplicate rows in the data frame based on all columns
unique_data <- all_data %>%
  distinct()

# Write the cleaned data frame to a new CSV file
write_csv(unique_data, "All_Chargers.csv")
