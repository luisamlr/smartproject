library(httr)
library(jsonlite)

# Define API endpoint URL
api_url <- "https://api.openchargemap.io/v3/poi/"

# Define query parameters
params <- list(
  outputformat = "json",
  countrycode = "NL",
  maxresults = 10000, # Set maximum number of results to retrieve
  key = ".........." # Replace with your actual API key
)

# Send GET request and retrieve response content as JSON
response <- GET(api_url, query = params)
json_content <- content(response, as = "text")
charging_stations <- fromJSON(json_content)

# Print number of charging stations retrieved
cat("Retrieved", nrow(charging_stations$data), "charging stations.\n")

# Print first charging station
cat("First charging station:\n")
print(charging_stations$data[1, ])

# Write data to CSV file
write.csv(charging_stations$data, file = "jedziemy.csv")
