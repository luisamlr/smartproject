#################################################################
#### Smart Project: OpenStreet Map API - Potential Locations ####
#################################################################

"""
This Python script performs a series of tasks to gather, process, and store information related to parking spots and
nearby amenities in a specific geographic region. Is used to extract all the parking spots of Maastricht and then
extract the geographical info about the spot (postal code, city, country) and all the facilities around.
The results are saved in two files, one with the list of parking spots and their info, and the other with the
surrounding facilities.
"""

# Import necessary libraries
import overpy
import pandas as pd
import requests

# Define a function to query OpenStreetMap for parking amenities around given coordinates within a certain radius
def query_osm_parking_around_coordinates(latitude, longitude, radius):
    # Initialize the Overpass API client
    api = overpy.Overpass()

    # Define the Overpass QL query. This queries for nodes, ways, and relations tagged with "amenity=parking" within the radius of the provided coordinates
    overpass_query = f"""
    [out:json][timeout:25];
    (
        nwr["amenity"="parking"](around:{radius},{latitude},{longitude});
    );
    out center;
    """
    # Send the query and get the result
    result = api.query(overpass_query)

    return result


# Define a function to reverse geocode the provided coordinates to get the address
def get_address(lat, lon):
    # Make a GET request to the Nominatim API with the provided coordinates
    response = requests.get(f'https://nominatim.openstreetmap.org/reverse?format=json&lat={lat}&lon={lon}')

    # Parse the response JSON
    data = response.json()

    # Get the address object from the response data
    address = data.get('address')

    # Extract the postal code, city, and country from the address.
    # If 'city' is not present, 'town' or 'village' is used instead
    postal_code = address.get('postcode')
    city = address.get('city', address.get('town', address.get('village')))
    country = address.get('country')

    return postal_code, city, country


# Set the center of Maastricht
maastricht_center = (50.8514, 5.6910)

# Set the search radius in meters (for example, 10 km)
radius = 10000

# Query parking spots in Maastricht
osm_data = query_osm_parking_around_coordinates(*maastricht_center, radius)

# Extract parking spot coordinates and related data
parking_spots = []
for element in osm_data.nodes + osm_data.ways + osm_data.relations:
    # Check if the element is a Node or a Way/Relation
    lat = element.lat if isinstance(element, overpy.Node) else element.center_lat
    lon = element.lon if isinstance(element, overpy.Node) else element.center_lon

    # Get the address of the parking spot
    postal_code, city, country = get_address(lat, lon)

    # Append the parking spot information to the list
    parking_spots.append({
        'name': element.tags.get('name'),
        'latitude': lat,
        'longitude': lon,
        'postal_code': postal_code,
        'city': city,
        'country': country
    })

# Create a DataFrame from the parking_spots list
parking_spots_df = pd.DataFrame(parking_spots)

# Save the DataFrame as a CSV file
parking_spots_df.to_csv('parking_spots_maastricht.csv', index=False)

# Extract the coordinates
coordinates = parking_spots_df[['latitude', 'longitude']].values.tolist()


# Define a function to query OpenStreetMap for various amenities around given coordinates within a certain radius
def query_osm_around_coordinates(latitude, longitude, radius):
    # Initialize the Overpass API client
    api = overpy.Overpass()

    # Define the Overpass QL query. This queries for nodes, ways, and relations tagged with "shop", "amenity" or "leisure" within the radius of the provided coordinates
    overpass_query = f"""
    [out:json][timeout:25];
    (
      nwr["shop"](around:{radius},{latitude},{longitude});
      nwr["amenity"](around:{radius},{latitude},{longitude});
      nwr["leisure"](around:{radius},{latitude},{longitude});
    );
    out center;
    """
    # Send the query and get the result
    result = api.query(overpass_query)
    return result


# Set the search radius in meters
radius = 1000

# Initialize an empty list to store the results
facilities = []

# For each parking spot, query the facilities around it and append to the list
for lat, lon in coordinates:
    osm_data = query_osm_around_coordinates(lat, lon, radius)
    for node in osm_data.nodes:
        facility = {
            'charger_latitude': lat,
            'charger_longitude': lon,
            'name': node.tags.get('name'),
            'type': node.tags.get('amenity') or node.tags.get('shop') or node.tags.get('leisure'),
            'latitude': node.lat,
            'longitude': node.lon,
        }

        facilities.append(facility)

# Create a DataFrame with the facilities
facilities_df = pd.DataFrame(facilities)

# Save the DataFrame to a CSV file
facilities_df.to_csv('facilities_around_coordinates_parking.csv', index=False)