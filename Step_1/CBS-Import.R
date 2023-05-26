# ------------------------------------------------------------------------------
# This file performs data preparation for the CBS dataset to obtain values at 
# the postcode level. It imports the raw dataset from CBS, aggregates the data 
# at the 4-digit postcode level, and exports the processed data as an Excel 
# file. The exported file can then be imported into other files to join with 
# relevant data.
# ------------------------------------------------------------------------------

# Load required packages
library(tidyr)
library(dplyr)
library(readr)
library(jsonlite)
library(ggthemes)


##### Socioeconomic data ####
# Import observations (raw data)
obs_data<- read_delim("/Users/rogerpuertolas/Desktop/CBS DATA/Observations.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
# Obtain postal code
data_pc <- filter(obs_data, grepl("^PC", Measure))
data_pc<-cbind(WijkenEnBuurten = data_pc$WijkenEnBuurten, StringValue = as.numeric(data_pc$StringValue))
data_pc<-as.data.frame(data_pc)

# Filter out rows with missing values
obs_data <- obs_data %>% 
  drop_na(Value)

# Convert Value column to numeric and replace commas with dots
obs_data <- obs_data %>% 
  mutate(Value = as.numeric(gsub(",", ".", Value))) 

# Keep only columns 2 to 4 (Measure, Location, Value)
obs_data <- obs_data %>% 
  select(2:4)

# Import variable names and select columns of interest
v_names <- read_delim("/Users/rogerpuertolas/Desktop/CBS DATA/MeasureCodes.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
v_names<- v_names[,c(1,4)]
# Join data frames by Measure and select relevant columns
df_socio_eco <- obs_data %>%
  left_join(v_names, by = c("Measure" = "Identifier")) %>%
  select(- Measure) %>%
  pivot_wider(names_from = Title, values_from = Value)

# Obtain the postcode of each observation
data_pre <- left_join(df_socio_eco, as.data.frame(data_pc) , by="WijkenEnBuurten")
# Remove the observation that are at the city level, or at the state level.
data_pre <- filter(data_pre, !is.na(StringValue))
# Aggregate the data by taking the mean of the postcode 
data_pre <- data_pre %>%
  group_by(StringValue) %>%
  summarize_all(mean, na.rm = TRUE)

data_pre$StringValue <- as.numeric(data_pre$StringValue)

library(writexl)
# Export data frame to an Excel file
write_xlsx(final, "CBS.xlsx") 
