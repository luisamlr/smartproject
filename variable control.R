##################################################
# Smart Project Data Cleaning & Variable Control #
##################################################

#the last edit of this code/maintenance took place on:
last_edit <- "2023-05-28"

#reshaped_table <- read.csv("/Users/isamahler/Desktop/SmartRProject/reshaped_table.csv", header = TRUE, sep = ",")
#combined_data <- read.csv("/Users/isamahler/Desktop/SmartRProject/combined_data.csv")

## Step 1: loading relevant libraries and importing data
library(readxl)
library(dplyr)

reshaped_poi_locations <- read_excel("reshaped_poi_locations.xlsx")
lookup <- read_excel("Step_1/renaming.xlsx")

# Step 2: renaming columns according to the New_names column in the lookup file: 
# variables indicating facilities close to the charging stations are supposed to start with "Fac_"
#start_index <- match("charger_latitude", colnames(reshaped_poi_locations)) + 1
#end_index <- match("Rating", colnames(reshaped_poi_locations)) - 1
#colnames(reshaped_poi_locations)[start_index:end_index] <- paste0("Fac_", colnames(reshaped_poi_locations)[start_index:end_index])

# simple replacement of the column names vector not possible as other variables might enter the next time data is downloaded
# this will be taken care of after the first iteration of aggregations
#name_match = match(names(reshaped_poi_locations), lookup$Old_names)
#names(reshaped_poi_locations)[na.omit(name_match)] = lookup$New_names[!is.na(name_match)]


#### Step 3: Removal of irrelevant/unidentifiable variables & move important variables to the back
# Discard variables that we deem irrelevant/if we are unable to explain them or are unsure about their meanings
# We discard variables again below but with that being part of the "optional" section this is still necessary to ensure they are taken out
variables_to_remove<-c("Fac_afro","Fac_aq","Fac_awards","Fac_bell","Fac_disused","Fac_flo","Fac_general","Fac_grinding","Fac_information","Fac_mirror_setting_marks",
                     "Fac_model","Fac_modeling","Fac_new_age","Fac_no","Fac_safe","Fac_sun","Fac_toko","Fac_trophy","Fac_vacant","Fac_voucher","Fac_webshop","Fac_yes",
                     "Immigrants_western_total","Immigrants_other_total","Immigrants_Marokko","Immigrants_Dutch_Antilles_Aruba","Immigrants_Suriname","Immigrants_Turkey","Immigrants_other_non_western")

reshaped_poi_locations <- reshaped_poi_locations %>% 
  # Remove the specified columns from the dataframe
  select(-all_of(variables_to_remove)) %>%
  # Move #Fac_centre #Fac_charging_station and #Fac_fuel as individual variables to the back of the dataframe
  select(-"Fac_centre", -"Fac_charging_station", -"Fac_fuel", everything())

# # Add test column
# reshaped_poi_locations <- reshaped_poi_locations %>%
#   mutate(Fac_test_clumn = seq(1,1260)) %>%
#   select(1:2, Fac_test_clumn, everything())

### First Iteration of Aggregations: combine those columns that we know mean the same thing
#this is already done as part of Rado's code

### Second Iteration of Aggregations: combine into categories (OPTIONAL - this section could be commented out)
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


#### Step 4: Removing those columns that have occurrences in less than five charging stations
# Extracting the column names starting with "Fac_"
fac_columns <- grep("^Fac_|^FAC_", names(reshaped_poi_locations), value = TRUE)
# Counting occurrences of entries larger than 0 in each "Fac_" column
occurrences <- sapply(fac_columns, function(col) sum(reshaped_poi_locations[[col]] > 0))
# Identifying the "Fac_" columns with less than 5 occurrences of entries larger than 0
low_occurrence_columns <- fac_columns[occurrences < 5]
# Removing the columns from the dataframe where less than five charging stations have the feature in their vicinity
reshaped_poi_locations <- reshaped_poi_locations %>% select(-all_of(low_occurrence_columns))


# Figure out if there are unaggregated variables left between the charging station coordinates and "Rating" 
if (num_columns_between > 0) {
  date <- last_edit  # Replace with the actual last edit date
  message <- paste("There are", num_columns_between, "columns that have not been categorized and will not be used in our model. The last edit of the code took place on", last_edit, ".")
  print(message)
}
