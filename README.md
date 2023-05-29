# smartproject
Smart Project on EV vehicle charging station location decisions


Data sets:

All_Chargers.csv - This file contains all the chargers we obtained from Google API. The variables included per charging station are: Longitude, Latitude, Postcode, Adress, Opening Hours, Company, Phone number, Email, Rating, Amount of ratings...

CBS-Import: This file performs data preparation for the CBS dataset to obtain values at the postcode level. It imports the raw dataset from CBS, aggregates the data at the 4-digit postcode level, and exports the processed data as an Excel file. The exported file can then be imported into other files to join with relevant data.
