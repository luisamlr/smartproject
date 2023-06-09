##############################################################
#### Smart Project: Google Maps API - EV Chargers by Grid ####
##############################################################

"""
This script computes 8 different locations around a given point (north, south, east, west, northeast, northwest,
southeast, southwest), each located 5 km away. It then uses Google's Places API to fetch the nearby charging stations
for electric vehicles at each location. For each charging station, it collects various details and writes them to a
separate CSV file for each location.
"""


# Import required libraries
import csv
import requests
import time
import datetime
from geopy.distance import distance, Point

# Function to extract address components
def extract_address_component(components, target_type):
    for component in components:
        if target_type in component["types"]:
            return component["long_name"]
    return "N/A"

# Get the current timestamp to differentiate each extracted file.
timestamp = datetime.datetime.now().strftime("%d-%m-%Y_%H-%M-%S")

# Function to calculate 8 points (4 cardinal and 4 inter-cardinal points) around the given latitude and longitude
def calculate_locations(lat, lon):
    # Create a Point object with the given lat, lon coordinates
    original_location = Point(lat, lon)

    # Calculate distances in meters for North, South, East and West
    north_location = distance(kilometers=5).destination(original_location, 0)
    south_location = distance(kilometers=5).destination(original_location, 180)
    east_location = distance(kilometers=5).destination(original_location, 90)
    west_location = distance(kilometers=5).destination(original_location, 270)

    # Calculate distances in meters for intercardinals
    northeast_location = distance(kilometers=5).destination(original_location, 45)
    northwest_location = distance(kilometers=5).destination(original_location, 315)
    southeast_location = distance(kilometers=5).destination(original_location, 135)
    southwest_location = distance(kilometers=5).destination(original_location, 225)

    # Store the latitude and longitude of each location as a string in the format "latitude, longitude"
    locations = [f"{lat}, {lon}",
                 f"{north_location.latitude}, {north_location.longitude}",
                 f"{south_location.latitude}, {south_location.longitude}",
                 f"{east_location.latitude}, {east_location.longitude}",
                 f"{west_location.latitude}, {west_location.longitude}",
                 f"{northeast_location.latitude}, {northeast_location.longitude}",
                 f"{northwest_location.latitude}, {northwest_location.longitude}",
                 f"{southeast_location.latitude}, {southeast_location.longitude}",
                 f"{southwest_location.latitude}, {southwest_location.longitude}"]

    # Save the locations to a CSV file
    with open(f'locations_{timestamp}.csv', mode='w', newline='') as csv_file:
        writer = csv.writer(csv_file)
        writer.writerow(['Latitude', 'Longitude'])
        for location in locations:
            lat, lon = location.split(", ")
            writer.writerow([lat, lon])

    # Return the list of locations
    return locations


# Get the latitude and longitude of the location you want to calculate other locations for (this is manual input).
lat = 51.48976
lon = 3.6000129

# Calculate the locations
locations = calculate_locations(lat, lon)

# Define the endpoint URL
endpoint_url = "https://maps.googleapis.com/maps/api/place/nearbysearch/json"
place_details_url = "https://maps.googleapis.com/maps/api/place/details/json"

# Define the parameters for the search
params = {
    "radius": "2000",
    "keyword": "electric vehicle charging station",
    "key": "YOUR API KEY HERE",  # Personal API key
}

# Loop through the locations and perform a search for each location
for idx, location in enumerate(locations):
    charging_stations = []
    params["location"] = location

    if "pagetoken" in params:
        del params["pagetoken"]  # Reset the pagetoken before starting the loop for the next location

    response = requests.get(endpoint_url, params=params)

    while True:
        for result in response.json()["results"]:
            name = result["name"]
            lat = result["geometry"]["location"]["lat"]
            lng = result["geometry"]["location"]["lng"]
            rating = result.get("rating", "N/A")
            address = result.get("formatted_address", "N/A")
            reviews = result.get("reviews", [])
            ratings_total = result.get("user_ratings_total", [])

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

            charging_stations.append((name, lat, lng, address, rating, reviews, ratings_total, postal_code, street_number, route, locality, admin_area_level_1, admin_area_level_2, country, phone_number, website, opening_hours))

        next_page_token = response.json().get("next_page_token", None)
        if not next_page_token:
            break

        time.sleep(2)  # Modify the waiting time if necessary
        params["pagetoken"] = next_page_token
        response = requests.get(endpoint_url, params=params)

    # Save the list of charging station data to a CSV file
    with open(f"charging_stations_gmaps_{idx}_{timestamp}.csv", "w", newline="") as csv_file:
        writer = csv.writer(csv_file, delimiter=";")
        writer.writerow(["Name", "Latitude", "Longitude", "Address", "Rating", "Reviews", "Ratings Total", "Postal Code", "Street Number", "Route", "Locality", "Admin Area Level 1", "Admin Area Level 2", "Country", "Phone Number", "Website", "Opening Hours"])
        writer.writerows(charging_stations)

        # Clear the charging_stations list for the next location
        charging_stations = []
