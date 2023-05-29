################################################################
#### Smart Project: Data Merge, Cleaning & Variable Control ####
################################################################

# Objective: Consolidation of Data from Diverse Sources

#### MAINTENANCE INSTRUCTIONS ####
#the last edit of this code took place on: (please update)
last_edit <- "2023-05-28"

### to-dos: ###
##check the number of variables that have not been sorted into categories:
#Step 1. comment in the loops (lines 127-141, lines 157-162, lines 177-209)
#Step 2. run this code once (takes approx 28 minutes)
#Step 3. check num_columns_between
#Step 4. if num_columns_between is above 0, then open file "reshaped_poi_locations" from the environment
#Step 5. find out the names of the columns that show up behind "charger_latitude" and before "Rating"
#Step 6. sort these variables manually into one of the categories (starting in line 299)
################################################################
#### Smart Project: Data Merge, Cleaning & Variable Control ####
################################################################

# Objective: Consolidation of Data from Diverse Sources

#### MAINTENANCE INSTRUCTIONS ####
#the last edit of this code took place on: (please update)
last_edit <- "2023-05-28"

### to-dos: ###
##check the number of variables that have not been sorted into categories:
#Step 1. comment in the loops (lines 127-141, lines 157-162, lines 177-209)
#Step 2. run this code once (takes approx 28 minutes)
#Step 3. check num_columns_between
#Step 4. if num_columns_between is above 0, then open file "reshaped_poi_locations" from the environment
#Step 5. find out the names of the columns that show up behind "charger_latitude" and before "Rating"
#Step 6. sort these variables manually into one of the categories (starting in line 299)
#Alternatively:
# if you want to create a new category, also make sure to include it in the dplyr function in line 357
# if you wish to instead remove the variable add it into the vector in line 287
#Step 7. run code again & repeat to check if anything was missed
#Step 8. comment the loops (lines 127-141, lines 157-162, lines 177-209) back out

#### Step 1: loading relevant libraries and importing data ####
## Install and load the required packages
library(caTools)
library(randomForest)
library(dplyr)
library(readxl)
library(reshape2)
library(openxlsx)
library(writexl)
library(ranger)
library(caret)
library(e1071)
library(tidyr)
library(readr)
library(geosphere)

## Setting the working directory ##
#setwd("C:/Users/radok/OneDrive/Desktop/Maastricht Univeristy/Service Project/Business Analytics/smartproject") #setwd("~/Maastricht University/smartproject")
#setwd("~/Maastricht University/smartproject")
setwd("/Users/isamahler/Desktop/SmartRProject/smartproject_new/Step_1")


## Loading of the data sources ##
# All chargers locations, from Google Maps:
df_cs <- read.csv("All_Chargers.csv")
# Facilities around chargers, from OpenStreetMap:
poi_locations <- read_excel("facilities_around_coordinates.xlsx")
# Demographic information for every Dutch postal code, From CBS:
demog_data <- read_excel("CBS.xlsx")
# Maastricht parking spots, from Google maps:
parking_maastricht <- read_excel("parking_spots_maastricht.xlsx")
# Distance to the closes highway, from Google maps:
highway_dist <- read.csv("highway_dist.csv")
# Auxiliary file:
renaming <- read_excel("renaming.xlsx")


#### Step 2: Data cleaning ####

## GOOGLE MAPS ##
# Selecting only used columns
highway_dist <- select(highway_dist,charger_longitude,charger_latitude,highway) # selecting only used columns
# Selecting only those stations with review and inside the Netherlands
df_cs <- df_cs[df_cs$Ratings.Total>0,]
df_cs <- df_cs[df_cs$Country=="Netherlands" ,]
# Replace "North" with "Noord" and " " with "-" in the Admin.Area.Level.1 column
df_cs$Admin.Area.Level.1 <- gsub("North", "Noord", df_cs$Admin.Area.Level.1)
df_cs$Admin.Area.Level.1 <- gsub("South", "Zuid", df_cs$Admin.Area.Level.1)
df_cs$Admin.Area.Level.1 <- gsub(" ", "-", df_cs$Admin.Area.Level.1)
# unique(df_cs$Admin.Area.Level.1) # 12 provinces in Netherlands


## OPEN STREET MAP ##
poi_locations$type <- paste0("Fac_",poi_locations$type) # Add a prefix to improve handling.
poi_locations$type <- make.names(poi_locations$type) # Fix type names to avoid problem on R.
poi_locations$type <- ifelse(poi_locations$type %in% renaming$Old_names, 
                             renaming$New_names[match(poi_locations$type, renaming$Old_names)],
                             poi_locations$type) # Renaming POI types, fixing mislabeled names, and grouping similar named types.

## CBS STATISTICS NETHERLANDS ##
names(demog_data) <- make.names(names(demog_data)) # Fix column names to avoid problem on R.
demog_data_names <- colnames(demog_data) # Saving column names.
indices <- match(demog_data_names, renaming$Old_names) # Find the indices of demog_data_names in renaming$Old_names
demog_data_names <- ifelse(!is.na(indices), renaming$New_names[indices], demog_data_names) # If there's a match, replace with renaming$New_names, else keep the original name
names(demog_data) <- demog_data_names # Assign the new column names to demog_data
demog_data <- demog_data[, !(colnames(demog_data) == "WijkenEnBuurten")] # Clean one column that is completely null.


#### Step 3: calculating average ratings ####
### Finding the averages of the 3, 5 or 10 nearest charging stations

# Function to calculate distance between two coordinates using geosphere package
calc_distance <- function(lat1, lon1, lat2, lon2) {
  dist <- distHaversine(c(lon1, lat1), c(lon2, lat2))
  return(dist)
}

# Function to find the nearest charging stations based on distance
find_nearest <- function(lat, lon, df_cs, closest = 1) {
  df_cs$Distance <- sapply(1:nrow(df_cs), function(i) {
    if (df_cs$Latitude[i] == lat && df_cs$Longitude[i] == lon) {
      return(Inf) # setting distance to infinity for charging station at given location
    } else {
      return(calc_distance(lat, lon, df_cs$Latitude[i], df_cs$Longitude[i]))
    }
  })
  df_sorted <- df_cs[order(df_cs$Distance),]
  return(head(df_sorted, n=closest))
}

# Function to compute average rating of the nearest charging stations
rating_nearest <- function(lat, lon, df, closest_cs = 1) {
  nearest_stations <- find_nearest(lat, lon, df,closest_cs)
  avg_rating <- mean(nearest_stations$Rating)
  return(avg_rating)
}

# Starting empty dataframes for using in the next calculation.
n_rating <- c()
avg3 <- c()
avg5 <- c()
avg10 <- c()
# for (i in 1:nrow(df_cs)){
#   # Find the nearest rating per CS
#   n_rating[i]<-rating_nearest(df_cs$Latitude[i], df_cs$Longitude[i], df_cs,1)
#   # Find the avg 3 nearest rating per CS
#   avg3[i]<-rating_nearest(df_cs$Latitude[i], df_cs$Longitude[i], df_cs, 3)
#   # Find the avg 5 nearest rating per CS
#   avg5[i]<-rating_nearest(df_cs$Latitude[i], df_cs$Longitude[i], df_cs, 5)
#   # Find the avg 10 nearest rating per CS
#   avg10[i]<-rating_nearest(df_cs$Latitude[i], df_cs$Longitude[i], df_cs, 10)
#   print(paste0("Calculating info for charger ", i," of ", nrow(df_cs)))
# }
# write.csv(n_rating, "n_rating.csv", row.names = FALSE)
# write.csv(avg3, "avg3.csv", row.names = FALSE)
# write.csv(avg5, "avg5.csv", row.names = FALSE)
# write.csv(avg10, "avg10.csv", row.names = FALSE)

# If the above loop is commented out, use this output:
n_rating2_csv <- read.csv("n_rating.csv"); n_rating <- as.numeric(n_rating2_csv[[1]])
avg3_csv <- read.csv("avg3.csv"); avg3 <- as.numeric(avg3_csv[[1]]); avg3 <- round(avg3, 5)
avg5_csv <- read.csv("avg5.csv"); avg5 <- as.numeric(avg5_csv[[1]]); avg5 <- round(avg5, 5)
avg10_csv <- read.csv("avg10.csv"); avg10 <- as.numeric(avg10_csv[[1]]); avg10 <- round(avg10, 5)

# Finding the closest charging stations, save the distance
closest <- function(lat, lon, df_cs) {
  df_cs$Distance <- sapply(1:nrow(df_cs), function(i) calc_distance(lat, lon, df_cs$Latitude[i], df_cs$Longitude[i]))
  df_sorted <- df_cs[order(df_cs$Distance),]
  return(head(df_sorted$Distance, n=1))
}

cls <- c()
# for (i in 1:nrow(df_cs)){
#   # avg_rating_3_nearest function for Station1
#   cls[i]<-closest(df_cs$Latitude[i], df_cs$Longitude[i], df_cs)
#   print(paste0("Calculating info for charger ", i," of ", nrow(df_cs)))
# }
# write.csv(cls, "cls.csv", row.names = FALSE)

# If the above loop is commented out, use this output:
cls_csv <- read.csv("cls.csv"); cls <- as.numeric(cls_csv[[1]])

## Calculating how many stations are under 1000m, 500m and 200m
# Import the dataset ---- We work with the whole dataset since we do not care whether they have review or not in order to take into account
df_all <- read.csv("All_Chargers.csv")
df_all <- df_all[df_all$Country=="Netherlands" ,]

# initialize empty vector to store counts
counts_1000 <- c()
counts_500 <- c()
counts_250 <- c()
counts_100 <- c()
## loop over each row in the dataframe
# for (i in 1:nrow(df_all)) {
#   # check if the row has a rating
#   if ((df_all$Ratings.Total[i] > 0)) {
#     # check if the row has a rating
#     distances<-c()
#     # calculate distance between this row and all other rows
#     for (j in 1:nrow(df_all)){
#       distances[j]<- distHaversine(c(df_all$Longitude[i], df_all$Latitude[i]), c(df_all$Longitude[j], df_all$Latitude[j]))
#     }
#     # count how many distances are less than 1000m
#     count_1000 <- nrow(df_all[distances <= 1000,]) - 1 #Subtract 1 because otherwise we count the station itself
#     # count how many distances are less than 500m
#     count_500 <- nrow(df_all[distances <= 500,])  - 1 #Subtract 1 because otherwise we count the station itself
#     # count how many distances are less than 250m
#     count_250 <- nrow(df_all[distances <= 250,])  - 1 #Subtract 1 because otherwise we count the station itself
#     # count how many distances are less than 100m
#     count_100 <- nrow(df_all[distances <= 100,])  - 1 #Subtract 1 because otherwise we count the station itself
#     # add count to vector
#     counts_1000 <- c(counts_1000, count_1000)
#     # add count to vector
#     counts_500 <- c(counts_500, count_500)
#     # add count to vector
#     counts_250 <- c(counts_250, count_250)
#     # add count to vector
#     counts_100 <- c(counts_100, count_100)
#   }
#   print(paste0("Calculating info for charger ", i," of ", nrow(df_all)))
# }
# write.csv(counts_1000, "counts_1000.csv", row.names = FALSE)
# write.csv(counts_500, "counts_500.csv", row.names = FALSE)
# write.csv(counts_250, "counts_250.csv", row.names = FALSE)
# write.csv(counts_100, "counts_100.csv", row.names = FALSE)

# If the above loop is commented out, use this output:
counts_1000_csv <- read.csv("counts_1000.csv"); counts_1000 <- as.numeric(counts_1000_csv[[1]])
counts_500_csv <- read.csv("counts_500.csv"); counts_500 <- as.numeric(counts_500_csv[[1]])
counts_250_csv <- read.csv("counts_250.csv"); counts_250 <- as.numeric(counts_250_csv[[1]])
counts_100_csv <- read.csv("counts_100.csv"); counts_100 <- as.numeric(counts_100_csv[[1]])

# Merging with previous information
aggregate_rating <- cbind(df_cs, n_rating, avg3,avg5, avg10, counts_100, counts_250, counts_500, counts_1000)


#### Step 4: Further Cleaning of Data
## Eliminating duplicates
# ev_chargers_reviews <- distinct(ev_chargers_reviews)
poi_locations <- distinct(poi_locations)
demog_data <- distinct(demog_data)
parking_maastricht <- distinct(parking_maastricht)
aggregate_rating <- distinct(aggregate_rating)

## Standardization of decimal coordinates in all files, to avoid problems when joining information.
poi_locations$latitude <- round(as.numeric(poi_locations$latitude),7)
poi_locations$longitude <- round(as.numeric(poi_locations$longitude),7)
poi_locations$charger_latitude <- round(as.numeric(poi_locations$charger_latitude),7)
poi_locations$charger_longitude <- round(as.numeric(poi_locations$charger_longitude),7)
parking_maastricht$latitude <- round(as.numeric(parking_maastricht$latitude),7)
parking_maastricht$longitude <- round(as.numeric(parking_maastricht$longitude),7)
aggregate_rating$Latitude <- round(as.numeric(aggregate_rating$Latitude),7)
aggregate_rating$Longitude <- round(as.numeric(aggregate_rating$Longitude),7)

## Additional data format corrections
demog_data$StringValue <- as.character(demog_data$StringValue)

## Calculating the distance between chargers and points of interest.
poi_locations$distance <- mapply(calc_distance, poi_locations$charger_latitude, poi_locations$charger_longitude, poi_locations$latitude, poi_locations$longitude)
poi_locations$distance <- as.numeric(poi_locations$distance)

## Ensuring that the key columns in both data frames have the same names.
names(demog_data)[names(demog_data) == "Longitude"] <- "charger_longitude"
names(demog_data)[names(demog_data) == "Latitude"] <- "charger_latitude"
names(aggregate_rating)[names(aggregate_rating) == "Longitude"] <- "charger_longitude"
names(aggregate_rating)[names(aggregate_rating) == "Latitude"] <- "charger_latitude"

## Counting the number of facilities around each charger, and reshaping this information horizontally (1 row = 1 charger).
reshaped_poi_locations <- poi_locations %>%
  group_by(charger_longitude, charger_latitude, type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  spread(key = type, value = count, fill = 0)

## Adding charger information from Google Maps (reviews, ratings and geographical information)
reshaped_poi_locations <- reshaped_poi_locations %>%
  left_join(aggregate_rating, by = c("charger_longitude" = "charger_longitude", "charger_latitude" = "charger_latitude"))

## Creating a trimmed postal code with only the first four characters to be compatible with the base from CBS.
reshaped_poi_locations$postal_trim <- substr(reshaped_poi_locations$`Postal.Code`, start = 1, stop = 4)

## Left join the demog_data from CSB
reshaped_poi_locations <- reshaped_poi_locations %>%
  left_join(demog_data, by = c("postal_trim"="StringValue"))

## Left join the highway_data from Google maps
reshaped_poi_locations <- reshaped_poi_locations %>%
  left_join(highway_dist, by = c("charger_longitude" = "charger_longitude", "charger_latitude" = "charger_latitude"))

## Clean one column that is completely null (redundant for current version, could be important later)
#reshaped_poi_locations <- reshaped_poi_locations[, !(colnames(reshaped_poi_locations) == "WijkenEnBuurten")]

## Fixing variable names, to avoid problem with columns names starting with numbers. Cleaning rows with NAs.
names(reshaped_poi_locations) <- make.names(names(reshaped_poi_locations))
reshaped_poi_locations <- na.omit(reshaped_poi_locations)

# Before saving the file to be use in model creations steps, is neccesary to remove an
reshaped_poi_locations_old <- select(reshaped_poi_locations, -Name, -Address, -Reviews, -Ratings.Total, -Postal.Code, -Street.Number, -Route, -Locality, -Admin.Area.Level.1, -Admin.Area.Level.2, -Country, -Phone.Number, -Website, -Opening.Hours, -postal_trim)

#### Step 5: Removal of irrelevant/unidentifiable variables & move important variables to the back
# Discard variables that we deem irrelevant/if we are unable to explain them or are unsure about their meanings
# We discard variables again below but with that being part of the "optional" section this is still necessary to ensure they are taken out
variables_to_remove <- c("Name","Address","Reviews","Ratings.Total","Postal.Code","Street.Number","Route","Locality","Admin.Area.Level.1","Admin.Area.Level.2","Country","Phone.Number","Website","Opening.Hours","postal_trim",
                         "Fac_afro","Fac_aq","Fac_awards","Fac_bell","Fac_disused","Fac_flo","Fac_general","Fac_grinding","Fac_information","Fac_mirror_setting_marks",
                         "Fac_model","Fac_modeling","Fac_new_age","Fac_no","Fac_safe","Fac_sun","Fac_toko","Fac_trophy","Fac_vacant","Fac_voucher","Fac_webshop","Fac_yes",
                         "Immigrants_western_total","Immigrants_other_total","Immigrants_Marokko","Immigrants_Dutch_Antilles_Aruba","Immigrants_Suriname","Immigrants_Turkey","Immigrants_other_non_western")

reshaped_poi_locations <- reshaped_poi_locations %>% 
  # Remove the specified columns from the dataframe
  select(-all_of(variables_to_remove)) %>%
  # Move #Fac_centre #Fac_charging_station and #Fac_fuel as individual variables to the back of the dataframe
  select(-"Fac_centre", -"Fac_charging_station", -"Fac_fuel", everything())


#### Step 6: Aggregations: combine into categories (OPTIONAL - this section could be commented out)
## Define assortments of variables
# 1: family
FAC_family_establishments <- c("Fac_animal_boarding","Fac_animal_training","Fac_baby","Fac_baby_hatch","Fac_basis_school","Fac_childcare","Fac_animal_ambulance","Fac_dog_grooming","Fac_dog_park","Fac_dog_toilet","Fac_indoor_play","Fac_kids_area","Fac_kindergarten","Fac_maternity_care","Fac_natal_care","Fac_obstetrician","Fac_pet","Fac_pet_grooming","Fac_playground","Fac_pregnancy","Fac_rescue_station",
                               "Fac_scout_camp","Fac_scout_hall","Fac_scout_hut","Fac_shelter","Fac_toys","Fac_obstetrician_var","Fac_veterinary")
# 2: gym
FAC_fitness <- c("Fac_bodytec","Fac_fitness_centre","Fac_fitness_station","Fac_gym","Fac_sport_centre","Fac_sports_centre","Fac_sports_hall")
# 3: sports # extreme sports
FAC_sporting <- c("Fac_sports","Fac_club","Fac_clubhouse","Fac_dance","Fac_dancing_school","Fac_disc_golf","Fac_dojo","Fac_hockey","Fac_sport","Fac_tennis","Fac_training","Fac_trampoline_park","Fac_water_sports","Fac_yoga","Fac_yoga_studio","Fac_airsoft","Fac_dive_centre")
# 4: hospitality
FAC_hospitality <- c("Fac_bakery","Fac_bar","Fac_bbq","Fac_beergarden","Fac_brewery","Fac_cafe","Fac_canteen","Fac_catering","Fac_coffee_tea","Fac_confectionery","Fac_cookery","Fac_fast_food","Fac_food","Fac_food_court","Fac_ice_cream","Fac_internet_cafe","Fac_juice_bar","Fac_jukebox","Fac_karaoke_box","Fac_restaurant","Fac_seafood","Fac_vending_machine")
# 5: groceries
FAC_groceries <- c("Fac_beverages","Fac_butcher","Fac_cheese","Fac_chocolate","Fac_dairy","Fac_delicatessen_store","Fac_dietary_supplement","Fac_farm","Fac_fruit","Fac_grocery","Fac_nutrition_supplements","Fac_nuts","Fac_pastry","Fac_pastry_supplies","Fac_peanut_butter","Fac_pizza","Fac_spices","Fac_supermarket","Fac_supplements","Fac_sweets")
# 6: organic/green living
FAC_organic <- c("Fac_bakery_greengrocer","Fac_butcher_greengrocer","Fac_clothes_second_hand","Fac_fairtrade","Fac_greengrocer","Fac_health_food","Fac_zero_waste_lifestyle")
# 7: convention centres
FAC_convention_centres <- c("Fac_art","Fac_art_gallery","Fac_arts_centre","Fac_concert_hall","Fac_conference_centre","Fac_events","Fac_exhibition_centre")
# 8: recreational establishments # includes vacation stuff
FAC_recreational_establishments <- c("Fac_amusement_arcade","Fac_bait","Fac_beach_resort","Fac_bowling_alley","Fac_camping","Fac_candles","Fac_cinema","Fac_comics","Fac_cooking","Fac_cooking_workshop","Fac_costume","Fac_escape_game","Fac_games","Fac_hobby","Fac_hoop","Fac_hunting_stand","Fac_ice_rink","Fac_luggage_locker","Fac_massage","Fac_meditation_centre","Fac_miniature_golf","Fac_movie_rental","Fac_music",
                                     "Fac_music_venue","Fac_party","Fac_photo","Fac_photo_booth","Fac_photography","Fac_piano","Fac_pitch","Fac_reception_desk","Fac_resort","Fac_resthouse","Fac_road_travel","Fac_swimming_area","Fac_theatre","Fac_ticket","Fac_ticket_validator","Fac_tourism","Fac_tourist","Fac_travel_agency","Fac_travel_goods","Fac_travel_supplies","Fac_video","Fac_video_games","Fac_water_park")
# 9: financial establishments/banking
FAC_financial_establishments <- c("Fac_atm","Fac_bank","Fac_bitcoin","Fac_currency_exchange","Fac_financial_advice","Fac_money_lender","Fac_money_transfer","Fac_mortgage","Fac_mortgage_bank","Fac_mortgage_broker","Fac_pawnbroker","Fac_payment_terminal")
# 10: clothing
FAC_clothing <- c("Fac_accessories","Fac_bag","Fac_boutique","Fac_clothes","Fac_clothes_gift","Fac_clothes_repair","Fac_clothes_interior_decoration","Fac_clothing_rental","Fac_concepts","Fac_conceptstore","Fac_dressing_room","Fac_tailor_dry_cleaning","Fac_fashion","Fac_fashion_accessories","Fac_hat","Fac_jeweler","Fac_jewelry","Fac_second_hand","Fac_thrift_store","Fac_vintage")
# 11: home furnishing; construction/property management
FAC_home_furnishing <- c("Fac_appliance","Fac_bathroom_furnishing","Fac_bed","Fac_bed_curtain_fabric","Fac_bedding","Fac_blinds","Fac_carpet","Fac_carpet_furniture","Fac_curtain_paint","Fac_doorknobs","Fac_doors_glaziery","Fac_fixme","Fac_furniture","Fac_garden","Fac_garden_centre","Fac_garden_furniture","Fac_household_goods","Fac_houseware",
                         "Fac_houseware_gift","Fac_interior_decoration","Fac_kitchen","Fac_kitchen_bathroom_furnishing","Fac_kitchen_equipment","Fac_kitchenware","Fac_lamps","Fac_laundry","Fac_lighting","Fac_lounger","Fac_paint","Fac_plumbing","Fac_tiles","Fac_vacuum_cleaner","Fac_wallpaper","Fac_washing_machine","Fac_white_goods","Fac_window_blind_curtain",
                         "Fac_building_materials","Fac_building_supplies","Fac_construction","Fac_electrical","Fac_energy","Fac_estate_agent","Fac_floor","Fac_flooring","Fac_floors","Fac_forklift","Fac_groundskeeping","Fac_heating","Fac_power_supply","Fac_sun_control","Fac_tool_hire","Fac_wood","Fac_wool")
# 12: luxury goods,sports and establishments; arts and collectibles; boating
FAC_luxury_sports_goods <- c("Fac_angling","Fac_angling_club","Fac_equestrian","Fac_fishing","Fac_golf","Fac_horse","Fac_horse_riding","Fac_antiques","Fac_auction","Fac_auction_house","Fac_collector","Fac_gold","Fac_gold_buyer","Fac_goldsmith","Fac_medal","Fac_boat","Fac_boat_rental","Fac_boat_storage","Fac_chandler","Fac_harbourmaster","Fac_marina","Fac_maritime","Fac_pedalo","Fac_sailing_school","Fac_scuba_diving","Fac_ship_chandler","Fac_slipway")
# 13: diy/workshops
FAC_diy_workshops <- c("Fac_3d_printing","Fac_art_supplies","Fac_knitting_crocheting","Fac_crafts","Fac_design","Fac_doityourself_hardware","Fac_fablab","Fac_fabric","Fac_frame","Fac_fur","Fac_haberdashery","Fac_hackerspace","Fac_handicraft","Fac_hardware","Fac_knitting_school","Fac_leather","Fac_pottery","Fac_sewing","Fac_textile","Fac_textile_printing","Fac_workshop")
# 14: (other) retail establishments; convenience stores; cosmetics
FAC_retail_establishments <- c("Fac_board_games","Fac_books","Fac_brewing_supplies","Fac_chemist","Fac_christmas","Fac_clocks","Fac_cookware","Fac_country_store","Fac_crystal","Fac_department_store","Fac_fireworks","Fac_florist","Fac_mall","Fac_merch","Fac_musical_instrument","Fac_newsagent","Fac_office_supplies","Fac_plants","Fac_pyrotechnics","Fac_seeds","Fac_shoe_repair_cleaning","Fac_shoelaces_shop",
                               "Fac_shoes","Fac_soap","Fac_spiritual","Fac_stationery","Fac_sun_shade","Fac_sunglasses","Fac_sunscreens","Fac_surfboards","Fac_watches","Fac_convenience","Fac_kiosk","Fac_variety_store","Fac_gift","Fac_give_box","Fac_charity","Fac_food_gift","Fac_shoes_gift_books","Fac_souvenir")
# 15: electronics
FAC_electronics_appliances <- c("Fac_business_machines","Fac_camera","Fac_communication","Fac_computer","Fac_computer_repair","Fac_device_charging_station","Fac_electronics","Fac_electronics_repair","Fac_emergency_service","Fac_hifi","Fac_hoover","Fac_mobile_equipment","Fac_mobile_phone","Fac_mobile_phone_repair","Fac_phone","Fac_radiotechnics","Fac_telecommunication","Fac_telephone")
# 16: cosmetics
FAC_cosmetics_establishments <- c("Fac_accupuncture","Fac_beauty","Fac_beauty_salon","Fac_cosmetics","Fac_hair_extensions","Fac_hairdresser","Fac_hairdresser_supply","Fac_makeup","Fac_nails","Fac_pearcing","Fac_pedicure","Fac_perfumery","Fac_piercing","Fac_sauna","Fac_spa","Fac_tanning","Fac_tanning_salon","Fac_tattoo","Fac_teeth_bleaching","Fac_wellness","Fac_wigs")
# 17: adult ("alcohol","gambling","erotica")
FAC_adult <- c("Fac_adult_gaming_centre","Fac_alcohol","Fac_bookmaker","Fac_brothel","Fac_cannabis","Fac_casino","Fac_cigar","Fac_coffeeshop","Fac_curiosa","Fac_e_cigarette","Fac_erotic","Fac_growshop","Fac_hemp_products","Fac_hookah_lounge","Fac_lingerie","Fac_lottery","Fac_marihuana","Fac_nightclub","Fac_drugs_paraphernalia","Fac_psychedelics","Fac_pub","Fac_sex","Fac_shisha","Fac_smartshop","Fac_smoking_area","Fac_soft_drugs","Fac_stripclub","Fac_swingerclub",
               "Fac_tobacco","Fac_vapestore","Fac_weapons","Fac_wine")
# 18: operations # all kinds of offices and operative establishments","storage","rent and repair
FAC_operations <- c("Fac_copyshop","Fac_delivery","Fac_printshop","Fac_housing","Fac_ink","Fac_loading_dock","Fac_locksmith","Fac_machinery_rental","Fac_machines","Fac_mail","Fac_meeting_room","Fac_music_studio","Fac_office","Fac_printer","Fac_printer_ink","Fac_printing","Fac_prison","Fac_rehearsal_studio","Fac_rental","Fac_repair","Fac_restaurant_supplies","Fac_shop_supplies","Fac_solar_panel",
                    "Fac_stamps","Fac_storage_rental","Fac_studio","Fac_track","Fac_tractor","Fac_trade","Fac_trailer","Fac_trolley_bay","Fac_valuer","Fac_village_pump","Fac_waiting_room","Fac_warehouse","Fac_weighbridge","Fac_wholesale","Fac_wholesale_seafood","Fac_wholesale_trade")
# 19: healthcare
FAC_healthcare <- c("Fac_audiologist","Fac_blood_bank","Fac_chiropractic","Fac_clinic","Fac_coffin","Fac_crematorium","Fac_dental_hygienist","Fac_dentist","Fac_doctors","Fac_exercise_therapy","Fac_first_aid","Fac_funeral_services","Fac_health","Fac_healthcare_var","Fac_health_centre","Fac_health_post","Fac_hearing_aids","Fac_herbalist","Fac_hospital","Fac_kinesiologist","Fac_laser_clinic","Fac_medical",
                    "Fac_medical_supply","Fac_mortuary","Fac_nutritionist","Fac_optical_instruments","Fac_optician","Fac_optician_hearing_aids","Fac_orthodontist","Fac_pharmacy","Fac_physical_therapy","Fac_physiotherapist","Fac_place_of_mourning","Fac_podologist_practice","Fac_skin_specialist","Fac_taxidermist","Fac_geriatric","Fac_homecare","Fac_nursing_home")
# 20: educational establishments
FAC_educational_establishments <- c("Fac_college","Fac_cooking_school","Fac_driving_school","Fac_educational_institution","Fac_homework_support","Fac_language_school","Fac_library","Fac_library_dropoff","Fac_music_school","Fac_prep_school","Fac_private_tutor","Fac_research_institute","Fac_boarding_school","Fac_school","Fac_sport_school","Fac_tailor_school","Fac_university")
# 21: public establishments # publicly accessible spaces
FAC_public_establishments <- c("Fac_agrarian","Fac_animal_shelter","Fac_archive","Fac_bandstand","Fac_bench_waste","Fac_binoculars","Fac_bird_hide","Fac_bus_station","Fac_centre_democrats","Fac_chair","Fac_churchbell","Fac_cloakroom","Fac_common","Fac_community_centre","Fac_compressed_air","Fac_courthouse","Fac_coworking_space","Fac_cultural_centre","Fac_drinking_water","Fac_fire_station","Fac_fireplace","Fac_fountain","Fac_grit_bin","Fac_hammock","Fac_letter_box","Fac_lifeboat",
                               "Fac_lifeboat_station","Fac_lifevest","Fac_lock","Fac_lockers","Fac_lost_found","Fac_market","Fac_marketplace","Fac_meeting_point","Fac_military_surplus","Fac_monastery","Fac_monument","Fac_natural_stone","Fac_nature_reserve","Fac_outdoor","Fac_outdoor_sports","Fac_outdoor_furniture","Fac_outdoor_seating","Fac_outpost","Fac_park","Fac_parking","Fac_parking_fuel","Fac_parking_entrance","Fac_parking_exit","Fac_parking_space",
                               "Fac_picnic_table","Fac_place_of_worship","Fac_police","Fac_post_box_parcel_locker","Fac_post_depot","Fac_post_office","Fac_public_bookcase","Fac_public_building","Fac_recycling","Fac_refugee_site","Fac_religion","Fac_rocks","Fac_sanitary_dump_station","Fac_security","Fac_shower","Fac_sign","Fac_social_centre","Fac_social_club","Fac_social_facility","Fac_station","Fac_stones","Fac_stool","Fac_table","Fac_table_tennis_table","Fac_toilets","Fac_tombstone",
                               "Fac_townhall","Fac_waste_disposal","Fac_waste_transfer_station","Fac_water","Fac_water_pipe","Fac_water_point","Fac_watering_place","Fac_wildlife_hide")
# 22: transport
FAC_transport <- c("Fac_bicycle","Fac_bicycle_parking","Fac_bicycle_rental","Fac_bicycle_repair","Fac_car","Fac_car_pooling","Fac_car_rental","Fac_car_repair_parts","Fac_car_sharing","Fac_car_wash","Fac_caravan","Fac_ferry_terminal","Fac_mobility_scooter","Fac_moped","Fac_moped_parking","Fac_mopeds","Fac_motorcycle","Fac_motorcycle_parking","Fac_motorcycle_rental","Fac_motorcycle_repair","Fac_scooter","Fac_spare_parts","Fac_taxi","Fac_truck","Fac_truck_wash","Fac_tyres","Fac_vehicle_inspection")

## Do the categorization using the vectors above
# Dplyr to mutate
reshaped_poi_locations <- reshaped_poi_locations %>%
  mutate(
    FAC_family_establishments = rowSums(select(reshaped_poi_locations, one_of(FAC_family_establishments))),
    FAC_fitness = rowSums(select(reshaped_poi_locations, one_of(FAC_fitness))),
    FAC_sporting = rowSums(select(reshaped_poi_locations, one_of(FAC_sporting))),
    FAC_hospitality = rowSums(select(reshaped_poi_locations, one_of(FAC_hospitality))),
    FAC_groceries = rowSums(select(reshaped_poi_locations, one_of(FAC_groceries))),
    FAC_organic = rowSums(select(reshaped_poi_locations, one_of(FAC_organic))),
    FAC_convention_centres = rowSums(select(reshaped_poi_locations, one_of(FAC_convention_centres))),
    FAC_recreational_establishments = rowSums(select(reshaped_poi_locations, one_of(FAC_recreational_establishments))),
    FAC_financial_establishments = rowSums(select(reshaped_poi_locations, one_of(FAC_financial_establishments))),
    FAC_clothing = rowSums(select(reshaped_poi_locations, one_of(FAC_clothing))),
    FAC_home_furnishing = rowSums(select(reshaped_poi_locations, one_of(FAC_home_furnishing))),
    FAC_luxury_sports_goods = rowSums(select(reshaped_poi_locations, one_of(FAC_luxury_sports_goods))),
    FAC_diy_workshops = rowSums(select(reshaped_poi_locations, one_of(FAC_diy_workshops))),
    FAC_retail_establishments = rowSums(select(reshaped_poi_locations, one_of(FAC_retail_establishments))),
    FAC_electronics_appliances = rowSums(select(reshaped_poi_locations, one_of(FAC_electronics_appliances))),
    FAC_cosmetics_establishments = rowSums(select(reshaped_poi_locations, one_of(FAC_cosmetics_establishments))),
    FAC_adult = rowSums(select(reshaped_poi_locations, one_of(FAC_adult))),
    FAC_operations = rowSums(select(reshaped_poi_locations, one_of(FAC_operations))),
    FAC_healthcare = rowSums(select(reshaped_poi_locations, one_of(FAC_healthcare))),
    FAC_educational_establishments = rowSums(select(reshaped_poi_locations, one_of(FAC_educational_establishments))),
    FAC_public_establishments = rowSums(select(reshaped_poi_locations, one_of(FAC_public_establishments))),
    FAC_transport = rowSums(select(reshaped_poi_locations, one_of(FAC_transport)))
  ) %>%
  select(
    -one_of(FAC_family_establishments,FAC_fitness,FAC_sporting,FAC_hospitality,FAC_groceries,FAC_organic,FAC_convention_centres,FAC_recreational_establishments,
            FAC_financial_establishments,FAC_clothing,FAC_home_furnishing,FAC_luxury_sports_goods,FAC_diy_workshops,FAC_retail_establishments,FAC_electronics_appliances,
            FAC_cosmetics_establishments,FAC_adult,FAC_operations,FAC_healthcare,FAC_educational_establishments,FAC_public_establishments,FAC_transport)
  )

# Figure out if there are unaggregated variables left between the charging station coordinates and "Rating"  
num_columns_between <- which(colnames(reshaped_poi_locations) == "Rating") - which(colnames(reshaped_poi_locations) == "charger_latitude") - 1

# We drop all the "Fac_" columns (anything before "Rating") by getting the index of the column 'Ratings' + determine all columns after 
columns_to_keep <- c("charger_longitude", "charger_latitude", "Rating", colnames(reshaped_poi_locations)[((which(colnames(reshaped_poi_locations) == "Rating")) + 1):ncol(reshaped_poi_locations)])
# Subset the dataframe to keep the desired columns
reshaped_poi_locations <- reshaped_poi_locations[, columns_to_keep, drop = FALSE]


#### Step 7: Removing those columns that have occurrences in less than five charging stations
# Extracting the column names starting with "Fac_"
fac_columns <- grep("^Fac_|^FAC_", names(reshaped_poi_locations), value = TRUE)
# Counting occurrences of entries larger than 0 in each "Fac_" column
occurrences <- sapply(fac_columns, function(col) sum(reshaped_poi_locations[[col]] > 0))
# Identifying the "Fac_" columns with less than 5 occurrences of entries larger than 0
low_occurrence_columns <- fac_columns[occurrences < 5]
# Removing the columns from the dataframe where less than five charging stations have the feature in their vicinity
reshaped_poi_locations <- reshaped_poi_locations %>% select(-all_of(low_occurrence_columns))

#### Step 8: Saving the data
# Save the data prepared for modeling creation
write_xlsx(reshaped_poi_locations, "reshaped_poi_locations.xlsx")
write_xlsx(reshaped_poi_locations_old, "reshaped_poi_locations_old.xlsx") #old version, just in case

#### Step 9: Maintenance
# Figure out if there are unaggregated variables left between the charging station coordinates and "Rating"  
if (num_columns_between > 0) {
  date <- last_edit  # Replace with the actual last edit date
  message <- paste("There are", num_columns_between, "columns that have not been categorized and will not be used in our model. The last edit of the code took place on", last_edit, ".")
  print(message)
}

