#######################################################
#### Smart Project: API Files Load and Preparation ####
#######################################################

# Set the working directory to the location of the script.
# On RStudio, the code below can be used. If not, please specify manually the folder where this script is located.
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# setwd("/path/to/your/script")

# Import necessary libraries
library(tidyverse)
library(readr)

# Set the path where the Gmaps files are located.
file_path <- "source_data/Gmaps_api/"

# List all files in the path
file_list <- list.files(file_path)

# Read each file in the list of files and append them into one data frame
# read_delim() is used with a semi-colon (;) delimiter to read the file
all_data <- lapply(paste0(file_path, file_list), function(file) {
  read_delim(file, col_types = cols(.default = "c"), delim = ';')  
}) %>% bind_rows() # bind_rows() combines all data frames in the list into a single data frame

# Remove duplicate rows in the data frame based on all columns
unique_data <- all_data %>%
  distinct()

unique_data <- data.frame(lapply(unique_data, function(x) iconv(x, "latin1", "ASCII", sub = "")))

# Write the output to the "output" folder
write_csv(unique_data, "output/All_Chargers.csv")
