library(httr)
library(jsonlite)
library(dplyr)

get_nearest_highway <- function(latitude, longitude) {
  overpass_api_url <- "https://overpass-api.de/api/interpreter"
  
  # List the highway types you are interested in (e.g., motorway, trunk, primary)
  highway_types <- c("motorway", "trunk", "primary")
  highway_filter <- paste0("highway~\"", paste(highway_types, collapse = "|"), "\"")
  
  overpass_query <- paste0(
    "[out:json];",
    "way[", highway_filter, "](around:1000,", latitude, ",", longitude, ");",
    "out geom;"
  )
  
  response <- POST(
    overpass_api_url,
    body = list(data = overpass_query),
    encode = "form"
  )
  
  if (response$status_code == 200) {
    content <- content(response, "text", encoding = "UTF-8")
    parsed <- fromJSON(content, flatten = TRUE)
    return(parsed)
  } else {
    warning("Request failed.")
    return(NULL)
  }
}

distance <- function(lat1, lon1, lat2, lon2) {
  r <- 6371000 # Earth's radius in meters
  phi1 <- lat1 * pi / 180
  phi2 <- lat2 * pi / 180
  delta_phi <- (lat2 - lat1) * pi / 180
  delta_lambda <- (lon2 - lon1) * pi / 180
  
  a <- sin(delta_phi / 2)^2 + cos(phi1) * cos(phi2) * sin(delta_lambda / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  return(r * c)
}
nearest_highway_distance <- function(latitude, longitude) {
  highways <- get_nearest_highway(latitude, longitude)
  
  if (is.null(highways)) {
    return(NA)
  }
  min_distance <- Inf
  if (is.null(nrow(highways$elements))) {
    return(1001)
  } else{
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
  
  return(min_distance)
}

highway_dist <- reshaped_poi_locations[,1:2]

highway_dist <- highway_dist %>%
  rowwise() %>%
  mutate(highway_dist = nearest_highway_distance(charger_latitude, charger_longitude))
highway_dist$highway <- ifelse(highway_dist$highway_dist <=1000, 1, 0)
write.csv(highway_dist, "highway_dist.csv")
