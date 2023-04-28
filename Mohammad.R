## Mohammad

library(httr)
library(jsonlite)
library(googleway)

key <- Sys.getenv("MY_API_KEY")

location <- "50.8514%2C5.6900"
radius <- 1000
keyword <- "charging"
url <- paste0("https://maps.googleapis.com/maps/api/place/nearbysearch/json?",
              "location=", location,
              "&radius=", radius,
              "&keyword=", keyword,
              "&key=", key)
response <- GET(url)
places <- fromJSON(rawToChar(response$content))
results <- places$results
