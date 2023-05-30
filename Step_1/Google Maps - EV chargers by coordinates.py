#####################################################################
#### Smart Project: Google Maps API - EV Chargers by Coordinates ####
#####################################################################

"""
This Python script uses the Google Places API to fetch nearby charging stations for electric vehicles at specified
locations. For each charging station found, it fetches details such as the station's name, location, ratings,
address, etc., and writes these details to a separate CSV file for each location.
Due to the limitations of the Google Places API, it returns a maximum of 20 results per request and divides these
results into pages. There is a limit of 3 pages per query, which means we can extract a maximum of 60 charging stations
per query. Therefore, repeated queries may be needed to obtain more results.
"""

# Import necessary libraries
import csv
import requests
import time

# Define the endpoint URL for Google Places API
endpoint_url = "https://maps.googleapis.com/maps/api/place/nearbysearch/json"
place_details_url = "https://maps.googleapis.com/maps/api/place/details/json"

# Define the locations for the search
locations = [
    "52.07667, 4.29861",
    "52.09083, 5.12222",
    "51.55551, 5.0913"
]

# Define the parameters for the search
params = {
    "radius": "20000",  # Search radius in meters (20 kilometers)
    "keyword": "electric vehicle charging station", # Search keyword
    "key": "API KEY",  # Google API Key
}

# Function to extract address components
def extract_address_component(components, target_type):
    for component in components:
        if target_type in component["types"]:
            return component["long_name"]
    return "N/A"

# Loop through the locations and perform a search for each location
for index, location in enumerate(locations):
    params["location"] = location
    response = requests.get(endpoint_url, params=params)
    charging_stations = []  # Initialize the charging_stations list for the current location

    while True:  # Loop to handle pagination in Google Places API
        for result in response.json()["results"]:  # Loop through each result in the response
            name = result["name"]
            lat = result["geometry"]["location"]["lat"]
            lng = result["geometry"]["location"]["lng"]
            rating = result.get("rating", "N/A")  # Get the rating, if not available, use "N/A"
            address = result.get("formatted_address", "N/A")  # Get the address, if not available, use "N/A"
            reviews = result.get("reviews", [])  # Get the reviews, if not available, use empty list
            ratings_total = result.get("user_ratings_total", [])  # Get the total number of ratings, if not available, use empty list

            # Get place details to obtain additional information
            place_id = result["place_id"]
            details_params = {
                "place_id": place_id,
                "key": params["key"],
                "fields": "address_component,formatted_phone_number,website,opening_hours",
            }
            details_response = requests.get(place_details_url, params=details_params)
            address_components = details_response.json()["result"]["address_components"]

            postal_code = extract_address_component(address_components, "postal_code")
            street_number = extract_address_component(address_components, "street_number")
            route = extract_address_component(address_components, "route")
            locality = extract_address_component(address_components, "locality")
            admin_area_level_1 = extract_address_component(address_components, "administrative_area_level_1")
            admin_area_level_2 = extract_address_component(address_components, "administrative_area_level_2")
            country = extract_address_component(address_components, "country")

            phone_number = details_response.json()["result"].get("formatted_phone_number", "N/A")
            website = details_response.json()["result"].get("website", "N/A")
            opening_hours = details_response.json()["result"].get("opening_hours", {}).get("weekday_text", "N/A")

            charging_stations.append((name, lat, lng, address, rating, reviews, ratings_total, postal_code,
                                      street_number, route, locality, admin_area_level_1, admin_area_level_2, country,
                                      phone_number, website, opening_hours))

        # Get the token for the next page of results
        next_page_token = response.json().get("next_page_token", None)
        if not next_page_token:
            break

        time.sleep(2)  # Wait for 2 seconds before making a new request to comply with Google's rate limits
        params["pagetoken"] = next_page_token
        response = requests.get(endpoint_url, params=params)

    # Save the list of charging station data to a CSV file for the current location
    with open(f"charging_stations_gmaps_{index}.csv", "w", newline="") as csv_file:
        writer = csv.writer(csv_file, delimiter=";")
        writer.writerow(
            ["Name", "Latitude", "Longitude", "Address", "Rating", "Reviews", "Ratings Total", "Postal Code",
             "Street Number", "Route", "Locality", "Admin Area Level 1", "Admin Area Level 2", "Country",
             "Phone Number", "Website", "Opening Hours"])
        writer.writerows(charging_stations)
