## Mohammad

library(httr)
library(jsonlite)
library(googleway)

key <- Sys.getenv("MY_API_KEY")

# Set the bounding box of Maastricht
# min_lat <- 50.7981
# max_lat <- 50.9030
# min_lon <- 5.6347
# max_lon <- 5.8309

min_lat <- 50.841423
max_lat <- 50.8581
min_lon <- 5.666937
max_lon <- 5.729

# Generate a grid of coordinates within the bounding box
n <- 20
latitudes <- seq(min_lat, max_lat, length.out = n)
longitudes <- seq(min_lon, max_lon, length.out = n)
coords <- expand.grid(latitudes, longitudes)


# Convert the grid to a data frame
locations <- data.frame(lat = coords[,1], lon = coords[,2])

radius <- 150
keyword <- "charging"

charging_points <- data.frame()
m <- c()

for (i in 1:nrow(locations)) {
  location <- paste0(locations[i,1], "%2C", locations[i,2])
  url <- paste0("https://maps.googleapis.com/maps/api/place/nearbysearch/json?",
                "location=", location,
                "&radius=", radius,
                "&keyword=", keyword,
                "&key=", key)
  response <- GET(url)
  places <- fromJSON(rawToChar(response$content))
  results <- places$results
  m <- c(m, nrow(results))
  if (!is.null(nrow(results))) {
    lat <- results$geometry$location$lat
    lng <- results$geometry$location$lng
    results <- cbind(lat, lng, results[, c("name", "place_id", "rating", "types", "user_ratings_total", "vicinity")])
    charging_points <- rbind(charging_points, results)
  }
}

all_centrum <- charging_points
all_centrum <- unique(all_centrum)
