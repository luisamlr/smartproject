import pandas as pd
import requests

# Set up Google Maps API key
api_key = "API KEY"  # Replace with your Google Maps API key


# Function to query Google Maps API and extract postcode
def get_postcode(latitude, longitude):
    url = f"https://maps.googleapis.com/maps/api/geocode/json?latlng={latitude},{longitude}&key={api_key}"
    response = requests.get(url)
    data = response.json()

    if "results" in data and len(data["results"]) > 0:
        address_components = data["results"][0]["address_components"]
        for component in address_components:
            if "postal_code" in component["types"]:
                return component["long_name"]

    return None


# Load the DataFrame with longitude and latitude coordinates
df = pd.read_csv("/Users/rogerpuertolas/Desktop/potential_parking_roger.csv", sep=";")

# Create new columns for storing location details
df["postcode"] = None
df["city"] = None
df["country"] = None

# Loop over each row in the DataFrame
for index, row in df.iterrows():
    longitude = float(row["charger_longitude"].replace(",", "."))
    latitude = float(row["charger_latitude"].replace(",", "."))

    # Query the Google Maps API to obtain location details
    response = requests.get(
        f"https://maps.googleapis.com/maps/api/geocode/json?latlng={latitude},{longitude}&key={api_key}")
    data = response.json()

    # Extract location details from the API response
    if "results" in data and len(data["results"]) > 0:
        for component in data["results"][0]["address_components"]:
            if "postal_code" in component["types"]:
                df.at[index, "postcode"] = component["long_name"]
            elif "locality" in component["types"]:
                df.at[index, "city"] = component["long_name"]
            elif "country" in component["types"]:
                df.at[index, "country"] = component["long_name"]

# Print the updated DataFrame
df["charger_longitude"] = df["charger_longitude"].str.replace(",", ".").astype(float)
df["charger_latitude"] = df["charger_latitude"].str.replace(",", ".").astype(float)
print(df)


df.to_excel("potential_charging_stations.xlsx", index=False)
