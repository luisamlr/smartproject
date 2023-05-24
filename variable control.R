##################################################
# Smart Project Data Cleaning & Variable Control #
##################################################

#reshaped_table <- read.csv("/Users/isamahler/Desktop/SmartRProject/reshaped_table.csv", header = TRUE, sep = ",")
#combined_data <- read.csv("/Users/isamahler/Desktop/SmartRProject/combined_data.csv")

## Step 1: loading relevant libraries and importing data
library(readxl)
library(dplyr)

reshaped_poi_locations <- read_excel("reshaped_poi_locations.xlsx")
reshaped_poi_locations$othertest <- NA
lookup <- read_excel("renaming.xlsx")

# Step 2: renaming columns according to the New_names column in the lookup file: 
# facilities close to the charging stations start with "Fac_"

# simple replacement of the column names vector not possible as other variables might enter the next time data is downloaded
# this will be taken care of after the first iteration of aggregations
# maybe we can also implement something where it only renames the columns before the "ratings" and shows us if there are other columns that still need to be renamed
# eg. "if there are columns before "ratings" that do not have "Fac_" in front of them, then they should be output, if there are 0, then it is fine"
name_match = match(names(reshaped_poi_locations), lookup$Old_names)
names(reshaped_poi_locations)[na.omit(name_match)] = lookup$New_names[!is.na(name_match)]


#### Step 3: Cleaning of data

# Discard variables that we deem irrelevant/if we are unable to explain them or are unsure about their meanings
new_columnnames <- colnames(reshaped_poi_locations) # create a vector to look at leftover columnnames
columns_to_remove <- c("Fac_afro", "Fac_aq", "Fac_awards", "Fac_bell", 
                       "Fac_disused", "Fac_flo", "Fac_general", "Fac_grinding", 
                       "Fac_information", "Fac_mirror_setting_marks", 
                       "Fac_model", "Fac_modeling", "Fac_new_age", 
                       "Fac_no", "Fac_safe", "Fac_sun", "Fac_vacant", 
                       "Fac_voucher", "Fac_webshop", "Fac_yes")
reshaped_poi_locations <- reshaped_poi_locations[, !(colnames(reshaped_poi_locations) %in% columns_to_remove)] # Remove the specified columns from the dataframe

### First Iteration of Aggregations: combine those columns that we know mean the same thing
reshaped_poi_locations$Fac_art_gallery<-reshaped_poi_locations$Fac_art_gallery+reshaped_poi_locations$Fac_artgallery;reshaped_poi_locations$Fac_artgallery<-NULL
reshaped_poi_locations$Fac_audiologist<-reshaped_poi_locations$Fac_audiologist+reshaped_poi_locations$Fac_audicien;reshaped_poi_locations$Fac_audicien<-NULL
reshaped_poi_locations$Fac_baby<-reshaped_poi_locations$Fac_baby+reshaped_poi_locations$Fac_baby_goods;reshaped_poi_locations$Fac_baby_goods<-NULL
reshaped_poi_locations$Fac_bag<-reshaped_poi_locations$Fac_bag+reshaped_poi_locations$Fac_bag_headware+reshaped_poi_locations$Fac_bag_jewelry;reshaped_poi_locations$Fac_bag_headware<-NULL;reshaped_poi_locations$Fac_bag_jewelry<-NULL
reshaped_poi_locations$Fac_bakery<-reshaped_poi_locations$Fac_bakery+reshaped_poi_locations$Fac_bakery_pastry;reshaped_poi_locations$Fac_bakery_pastry<-NULL
reshaped_poi_locations$Fac_bathroom_furnishing<-reshaped_poi_locations$Fac_bathroom_furnishing+reshaped_poi_locations$Fac_bathroom;reshaped_poi_locations$Fac_bathroom<-NULL
reshaped_poi_locations$Fac_beauty<-reshaped_poi_locations$Fac_beauty+reshaped_poi_locations$Fac_beauty_clothes+reshaped_poi_locations$Fac_beauty_products;reshaped_poi_locations$Fac_beauty_clothes<-NULL;reshaped_poi_locations$Fac_beauty_products<-NULL
reshaped_poi_locations$Fac_bench_waste<-reshaped_poi_locations$Fac_bench_waste+reshaped_poi_locations$Fac_bench+reshaped_poi_locations$Fac_waste_basket;reshaped_poi_locations$Fac_bench<-NULL;reshaped_poi_locations$Fac_waste_basket<-NULL
reshaped_poi_locations$Fac_bicycle_rental<-reshaped_poi_locations$Fac_bicycle_rental+reshaped_poi_locations$Fac_bicycle_parking_bicycle_rental;reshaped_poi_locations$Fac_bicycle_parking_bicycle_rental<-NULL
reshaped_poi_locations$Fac_bicycle_repair<-reshaped_poi_locations$Fac_bicycle_repair+reshaped_poi_locations$Fac_bicycle_repair_station+reshaped_poi_locations$Fac_bicycle_repair_station_cafe;reshaped_poi_locations$Fac_bicycle_repair_station<-NULL;reshaped_poi_locations$Fac_bicycle_repair_station_cafe<-NULL
reshaped_poi_locations$Fac_butcher_greengrocer<-reshaped_poi_locations$Fac_butcher_greengrocer+reshaped_poi_locations$Fac_butcher_greengrocer2;reshaped_poi_locations$Fac_butcher_greengrocer2<-NULL
reshaped_poi_locations$Fac_car_repair_parts<-reshaped_poi_locations$Fac_car_repair_parts+reshaped_poi_locations$Fac_car_car_repair+reshaped_poi_locations$Fac_car_carrepair+reshaped_poi_locations$Fac_car_parts+
  reshaped_poi_locations$Fac_car_parts_car_repair+reshaped_poi_locations$Fac_car_parts_tools+reshaped_poi_locations$Fac_car_repair+reshaped_poi_locations$Fac_car_supplies+
  reshaped_poi_locations$Fac_car_tires;reshaped_poi_locations$Fac_car_car_repair<-NULL;reshaped_poi_locations$Fac_car_carrepair<-NULL;reshaped_poi_locations$Fac_car_parts<-NULL;
reshaped_poi_locations$Fac_car_parts_car_repair<-NULL;reshaped_poi_locations$Fac_car_parts_tools<-NULL;reshaped_poi_locations$Fac_car_repair<-NULL;reshaped_poi_locations$Fac_car_supplies<-NULL;reshaped_poi_locations$Fac_car_tires<-NULL
reshaped_poi_locations$Fac_coffee_tea<-reshaped_poi_locations$Fac_coffee_tea+reshaped_poi_locations$Fac_coffee+reshaped_poi_locations$Fac_coffee_and_tea+reshaped_poi_locations$Fac_tea;reshaped_poi_locations$Fac_coffee<-NULL;reshaped_poi_locations$Fac_coffee_and_tea<-NULL;reshaped_poi_locations$Fac_tea<-NULL
reshaped_poi_locations$Fac_coffeeshop<-reshaped_poi_locations$Fac_coffeeshop+reshaped_poi_locations$Fac_coffee_shop;reshaped_poi_locations$Fac_coffee_shop<-NULL
reshaped_poi_locations$Fac_conference_centre<-reshaped_poi_locations$Fac_conference_centre+reshaped_poi_locations$Fac_conference_center;reshaped_poi_locations$Fac_conference_center<-NULL
reshaped_poi_locations$Fac_crafts<-reshaped_poi_locations$Fac_crafts+reshaped_poi_locations$Fac_craft;reshaped_poi_locations$Fac_craft<-NULL
reshaped_poi_locations$Fac_doityourself_hardware<-reshaped_poi_locations$Fac_doityourself_hardware+reshaped_poi_locations$Fac_doityourself;reshaped_poi_locations$Fac_doityourself<-NULL
reshaped_poi_locations$Fac_doors_glaziery<-reshaped_poi_locations$Fac_doors_glaziery+reshaped_poi_locations$Fac_doors+reshaped_poi_locations$Fac_deuren+reshaped_poi_locations$Fac_glaziery;reshaped_poi_locations$Fac_doors<-NULL;reshaped_poi_locations$Fac_deuren<-NULL;reshaped_poi_locations$Fac_glaziery<-NULL
reshaped_poi_locations$Fac_drugs_paraphernalia<-reshaped_poi_locations$Fac_drugs_paraphernalia+reshaped_poi_locations$Fac_drugs+reshaped_poi_locations$Fac_drug_paraphernalia;reshaped_poi_locations$Fac_drugs<-NULL;reshaped_poi_locations$Fac_drug_paraphernalia<-NULL
reshaped_poi_locations$Fac_dry_cleaning<-reshaped_poi_locations$Fac_dry_cleaning+reshaped_poi_locations$Fac_dry_cleaning_general+reshaped_poi_locations$Fac_dry_cleaning_tailor;reshaped_poi_locations$Fac_dry_cleaning_general<-NULL;reshaped_poi_locations$Fac_dry_cleaning_tailor<-NULL
reshaped_poi_locations$Fac_events<-reshaped_poi_locations$Fac_events+reshaped_poi_locations$Fac_event_centre+reshaped_poi_locations$Fac_events_venue;reshaped_poi_locations$Fac_event_centre<-NULL;reshaped_poi_locations$Fac_events_venue<-NULL
reshaped_poi_locations$Fac_furniture<-reshaped_poi_locations$Fac_furniture+reshaped_poi_locations$Fac_furniture_housing+reshaped_poi_locations$Fac_furniture_lighting+reshaped_poi_locations$Fac_furniture_rattan_wholesale+reshaped_poi_locations$Fac_furnitureshop;reshaped_poi_locations$Fac_furniture_housing<-NULL;reshaped_poi_locations$Fac_furniture_lighting<-NULL;reshaped_poi_locations$Fac_furniture_rattan_wholesale<-NULL;reshaped_poi_locations$Fac_furnitureshop<-NULL
reshaped_poi_locations$Fac_garden_furniture<-reshaped_poi_locations$Fac_garden_furniture+reshaped_poi_locations$Fac_garden_furniture_interior_decoration;reshaped_poi_locations$Fac_garden_furniture<-NULL;reshaped_poi_locations$Fac_garden_furniture_interior_decoration<-NULL
reshaped_poi_locations$Fac_golf<-reshaped_poi_locations$Fac_golf+reshaped_poi_locations$Fac_disc_golf_cource+reshaped_poi_locations$Fac_disc_golf_course;reshaped_poi_locations$Fac_disc_golf_cource<-NULL;reshaped_poi_locations$Fac_disc_golf_course<-NULL
reshaped_poi_locations$Fac_interior_decoration<-reshaped_poi_locations$Fac_interior_decoration+reshaped_poi_locations$Fac_interior_decoration_carpet_curtain_bed+reshaped_poi_locations$Fac_interior_decoration_tailor;reshaped_poi_locations$Fac_interior_decoration_carpet_curtain_bed<-NULL;reshaped_poi_locations$Fac_interior_decoration_tailor<-NULL
reshaped_poi_locations$Fac_mobile_phone_repair<-reshaped_poi_locations$Fac_mobile_phone_repair+reshaped_poi_locations$Fac_mobile_phone_repair2;reshaped_poi_locations$Fac_mobile_phone_repair2<-NULL
reshaped_poi_locations$Fac_sewing<-reshaped_poi_locations$Fac_sewing+reshaped_poi_locations$Fac_sewing_fabric+reshaped_poi_locations$Fac_sewing_machines;reshaped_poi_locations$Fac_sewing_fabric<-NULL;reshaped_poi_locations$Fac_sewing_machines<-NULL
reshaped_poi_locations$Fac_shoe_repair_cleaning<-reshaped_poi_locations$Fac_shoe_repair_cleaning+reshaped_poi_locations$Fac_shoe_cleaning+reshaped_poi_locations$Fac_shoe_repair+reshaped_poi_locations$Fac_shoe_repair_key_cutter_leather;reshaped_poi_locations$Fac_shoe_cleaning<-NULL;reshaped_poi_locations$Fac_shoe_repair<-NULL;reshaped_poi_locations$Fac_shoe_repair_key_cutter_leather<-NULL
reshaped_poi_locations$Fac_soft_drugs<-reshaped_poi_locations$Fac_soft_drugs+reshaped_poi_locations$Fac_softdrugs;reshaped_poi_locations$Fac_softdrugs<-NULL
reshaped_poi_locations$Fac_sunglasses<-reshaped_poi_locations$Fac_sunglasses+reshaped_poi_locations$Fac_sun_glasses;reshaped_poi_locations$Fac_sun_glasses<-NULL
reshaped_poi_locations$Fac_swimming_area<-reshaped_poi_locations$Fac_swimming_area+reshaped_poi_locations$Fac_swiming_area+reshaped_poi_locations$Fac_swimming_pool+reshaped_poi_locations$Fac_bathing_place;reshaped_poi_locations$Fac_swiming<-NULL;reshaped_poi_locations$Fac_swimming_pool<-NULL;reshaped_poi_locations$Fac_bathing_place<-NULL
reshaped_poi_locations$Fac_watches<-reshaped_poi_locations$Fac_watches+reshaped_poi_locations$Fac_watch;reshaped_poi_locations$Fac_watch<-NULL

### Second Iteration of Aggregations: combine into categories (OPTIONAL - this section could be commented out)
## Define assortments of variables

# 1: family
Fac_family_establishments <- c("Fac_animal_boarding","Fac_animal_training","Fac_baby","Fac_baby_hatch","Fac_basis_school","Fac_childcare","Fac_animal_ambulance","Fac_dog_grooming","Fac_dog_park","Fac_dog_toilet","Fac_indoor_play","Fac_kids_area","Fac_kindergarten","Fac_maternity_care","Fac_natal_care","Fac_obstetrician","Fac_pet","Fac_pet_grooming","Fac_playground","Fac_pregnancy","Fac_rescue_station",
                               "Fac_scout_camp","Fac_scout_hall","Fac_scout_hut","Fac_shelter","Fac_toys","Fac_toys_gift_watches","Fac_obstetrician","Fac_veterinary")
# 2: gym
Fac_fitness <- c("Fac_bodytec","Fac_fitness_centre","Fac_fitness_station","Fac_gym","Fac_sport_centre","Fac_sports_centre","Fac_sports_hall")
# 3: sport
Fac_sporting <- c("Fac_sports","Fac_club","Fac_clubhouse","Fac_dance","Fac_dancing_school","Fac_dojo","Fac_hockey","Fac_sport","Fac_tennis","Fac_training","Fac_trampoline_park","Fac_water_sports","Fac_yoga","Fac_yoga_studio")
# 4: hospitality
Fac_hospitality <- c("Fac_bakery","Fac_bar","Fac_bbq","Fac_beergarden","Fac_brewery","Fac_cafe","Fac_cafe_guest_house","Fac_canteen","Fac_catering","Fac_coffee_tea","Fac_confectionery","Fac_cookery","Fac_fast_food","Fac_food","Fac_food_court","Fac_ice_cream","Fac_internet_cafe","Fac_juice","Fac_juice_bar","Fac_jukebox","Fac_karaoke_box","Fac_restaurant","Fac_seafood","Fac_vending_machine")
# 5: organic/green living
Fac_organic <- c("Fac_bakery_greengrocer","Fac_butcher_greengrocer","Fac_clothes_second_hand","Fac_fairtrade","Fac_greengrocer","Fac_health_food","Fac_zero_waste_lifestyle")
# 6: convention centres
Fac_convention_centres <- c("Fac_art","Fac_art_gallery","Fac_arts_centre","Fac_concert_hall","Fac_conference_centre","Fac_events","Fac_exhibition_centre")
# 7: recreational establishments # includes vacation stuff
Fac_recreational_establishments <- c("Fac_amusement_arcade","Fac_bait","Fac_beach_resort","Fac_bowling_alley","Fac_camping","Fac_candles","Fac_cinema","Fac_comics","Fac_cooking","Fac_cooking_workshop","Fac_costume","Fac_escape_game","Fac_games","Fac_hobby","Fac_hoop","Fac_hunting_stand","Fac_ice_rink","Fac_luggage_locker","Fac_massage","Fac_meditation_centre","Fac_miniature_golf","Fac_movie_rental","Fac_music",
                                     "Fac_music_venue","Fac_party","Fac_photo","Fac_photo_booth","Fac_photography","Fac_piano","Fac_pitch","Fac_reception_desk","Fac_resort","Fac_resthouse","Fac_road_travel","Fac_swimming_area","Fac_theatre","Fac_ticket","Fac_ticket_validator","Fac_tourism","Fac_tourist","Fac_travel_agency","Fac_travel_goods","Fac_travel_supplies","Fac_video","Fac_video_games","Fac_water_park")
# 8: financial establishments/banking
Fac_financial_establishments <- c("Fac_bank","Fac_bitcoin","Fac_currency_exchange","Fac_financial_advice","Fac_money_lender","Fac_money_transfer","Fac_mortgage","Fac_mortgage_bank","Fac_mortgage_broker","Fac_pawnbroker","Fac_payment_terminal")
# 9: clothing
Fac_clothing <- c("Fac_accessories","Fac_bag","Fac_boutique","Fac_clothes","Fac_clothes_gift","Fac_clothes_repair","Fac_clothes_interior_decoration","Fac_clothing_rental","Fac_cloths","Fac_concepts","Fac_conceptstore","Fac_dressing_room","Fac_dry_cleaning","Fac_fashion","Fac_fashion_accessories","Fac_hat","Fac_jeweler","Fac_jewelry","Fac_second_hand","Fac_thrift_store","Fac_vintage")
# 10: home furnishing; construction/property management
Fac_home_furnishing <- c("Fac_appliance","Fac_bathroom_furnishing","Fac_bed","Fac_bed_curtain_fabric","Fac_bedding","Fac_blinds","Fac_carpet","Fac_carpet_furniture","Fac_curtain","Fac_curtain_paint","Fac_doorknobs","Fac_doors_glaziery","Fac_fixme","Fac_furniture","Fac_garden","Fac_garden_centre","Fac_garden_furniture","Fac_household","Fac_household_goods","Fac_household_linen","Fac_houseware",
                         "Fac_houseware_gift","Fac_interior_decoration","Fac_kitchen","Fac_kitchen_bathroom_furnishing","Fac_kitchen_equipment","Fac_kitchenware","Fac_lamps","Fac_laundry","Fac_lighting","Fac_lounger","Fac_paint","Fac_plumbing","Fac_tiles","Fac_vacuum_cleaner","Fac_wallpaper","Fac_washing_machine","Fac_white_goods","Fac_window_blind","Fac_window_blind_curtain",
                         "Fac_building_materials","Fac_building_supplies","Fac_construction","Fac_electrical","Fac_energy","Fac_estate_agent","Fac_floor","Fac_flooring","Fac_floors","Fac_forklift","Fac_glaziery","Fac_groundskeeping","Fac_heating","Fac_power_supply","Fac_sun_control","Fac_tool_hire","Fac_wood","Fac_wool")
# 11: luxury goods,sports and establishments; arts and collectibles; boating
Fac_luxury_sports_goods <- c("Fac_angling","Fac_angling_club","Fac_equestrian","Fac_fishing","Fac_golf","Fac_horse","Fac_horse_riding","Fac_antiques","Fac_auction","Fac_auction_house","Fac_collector","Fac_gold","Fac_gold_buyer","Fac_goldsmith","Fac_medal","Fac_boat","Fac_boat_rental","Fac_boat_storage","Fac_chandler","Fac_harbourmaster","Fac_marina","Fac_maritime","Fac_pedalo","Fac_sailing_school","Fac_scuba_diving","Fac_ship_chandler","Fac_slipway")
# 12: diy/workshops
Fac_diy_workshops <- c("Fac_3d_printing","Fac_art_supplies","Fac_knitting_crocheting","Fac_crafts","Fac_design","Fac_doityourself_hardware","Fac_fablab","Fac_fabric","Fac_frame","Fac_fur","Fac_haberdashery","Fac_hackerspace","Fac_handicraft","Fac_hardware","Fac_knitting_school","Fac_leather","Fac_pottery","Fac_sewing","Fac_tailor","Fac_tailor_dry_cleaning","Fac_textile","Fac_textile_printing","Fac_workshop")
# 13: (other) retail establishments; convenience stores; cosmetics
Fac_retail_establishments <- c("Fac_board_games","Fac_books","Fac_brewing_supplies","Fac_chemist","Fac_christmas","Fac_clock","Fac_clocks","Fac_cookware","Fac_country_store","Fac_crystal","Fac_department_store","Fac_fireworks","Fac_florist","Fac_flower_arranging","Fac_mall","Fac_merch","Fac_musical_instrument","Fac_newsagent","Fac_plants","Fac_pyrotechnics","Fac_seeds","Fac_shoe_repair_cleaning","Fac_shoelaces_shop",
                               "Fac_shoes","Fac_soap","Fac_spiritual","Fac_stationery","Fac_sun_glasses","Fac_sun_shade","Fac_sunglasses","Fac_sunscreens","Fac_surfboards","Fac_watches","Fac_convenience","Fac_kiosk","Fac_variety_store","Fac_gift","Fac_give_box")
# 14: electronics
Fac_electronics_appliances <- c("Fac_business_machines","Fac_camera","Fac_communication","Fac_computer","Fac_computer_repair","Fac_electronics","Fac_electronics_repair","Fac_emergency_service","Fac_hifi","Fac_hoover","Fac_mobile_equipment","Fac_mobile_phone","Fac_mobile_phone_repair","Fac_phone","Fac_radiotechnics","Fac_telecommunication","Fac_telephone")
# 15: cosmetics
Fac_cosmetics_establishments <- c("Fac_accupuncture","Fac_beauty","Fac_beauty_salon","Fac_cosmetics","Fac_hair_extensions","Fac_hairdresser","Fac_hairdresser_supply","Fac_makeup","Fac_nails","Fac_pearcing","Fac_pedicure","Fac_perfumery","Fac_piercing","Fac_sauna","Fac_spa","Fac_tanning","Fac_tanning_salon","Fac_tattoo","Fac_teeth_bleaching","Fac_teeth_whitening","Fac_wellness","Fac_wigs")
# 16: adult ("alcohol","gambling","erotica")
Fac_adult <- c("Fac_adult_gaming_centre","Fac_alcohol","Fac_bookmaker","Fac_brothel","Fac_cannabis","Fac_casino","Fac_coffeshop","Fac_curiosa","Fac_e_cigarette","Fac_erotic","Fac_growshop","Fac_hemp_products","Fac_hookah_lounge","Fac_lingerie","Fac_lottery","Fac_marihuana","Fac_nightclub","Fac_drugs_paraphernalia","Fac_psychedelics","Fac_pub","Fac_sex","Fac_shisha","Fac_smartshop","Fac_smoking_area","Fac_soft_drugs","Fac_stripclub","Fac_swingerclub",
               "Fac_tobacco","Fac_tobacco_gift","Fac_vapestore","Fac_weapons","Fac_wine")
# 17: operations # all kinds of offices and operative establishments","storage","rent and repair
Fac_operations <- c("Fac_delivery","Fac_printshop","Fac_housing","Fac_ink","Fac_loading_dock","Fac_locksmith","Fac_machinery_rental","Fac_machines","Fac_mail","Fac_meeting_centre","Fac_meeting_room","Fac_meetinglocation","Fac_music_studio","Fac_office","Fac_office_supplies","Fac_printer","Fac_printer_ink","Fac_printing","Fac_prison","Fac_rehearsal_studio","Fac_rental","Fac_repair","Fac_restaurant_supplies","Fac_shop_supplies","Fac_solar_panel",
                    "Fac_stamps","Fac_storage_rental","Fac_studio","Fac_studio_and_meeting","Fac_track","Fac_tractor","Fac_trade","Fac_trailer","Fac_trolley_bay","Fac_valuer","Fac_village_pump","Fac_waiting_room","Fac_warehouse","Fac_weighbridge","Fac_wholesale","Fac_wholesale_seafood","Fac_wholesale_trade")
# 18: healthcare
Fac_healthcare <- c("Fac_audiologist","Fac_blood_bank","Fac_chiropractic","Fac_clinic","Fac_coffin","Fac_crematorium","Fac_dental_hygienist","Fac_dentist","Fac_doctors","Fac_exercise_therapy","Fac_first_aid","Fac_funeral_directors","Fac_funeral_hall","Fac_funeral_home","Fac_health","Fac_healthcare_var","Fac_health_centre","Fac_health_post","Fac_hearing_aids","Fac_herbalist","Fac_hospital","Fac_kinesiologist","Fac_laser_clinic","Fac_medical",
                    "Fac_medical_supply","Fac_mortuary","Fac_nutritionist","Fac_optical_instruments","Fac_optician","Fac_optician_hearing_aids","Fac_orthodontist","Fac_pharmacy","Fac_physical_therapy","Fac_physiotherapist","Fac_place_of_mourning","Fac_podologist_practice","Fac_skin_specialist","Fac_taxidermist")

## Do the categorization using the vectors above

# Determine all columns after 'Ratings'
columns_after_ratings <- colnames(df)[(ratings_index + 1):ncol(df)]

# Dplyr to mutate
reshaped_poi_locations <- reshaped_poi_locations %>%
  mutate(
    Fac_family_establishments = rowSums(select(., one_of(Fac_family_establishments))),
    Fac_fitness = rowSums(select(., one_of(Fac_fitness))),
    Fac_sporting = rowSums(select(., one_of(Fac_sporting))),
    Fac_hospitality = rowSums(select(., one_of(Fac_hospitality))),
    Fac_organic = rowSums(select(., one_of(Fac_organic))),
    Fac_convention_centres = rowSums(select(., one_of(Fac_convention_centres))),
    Fac_recreational_establishments = rowSums(select(., one_of(Fac_recreational_establishments))),
    Fac_financial_establishments = rowSums(select(., one_of(Fac_financial_establishments))),
    Fac_clothing = rowSums(select(., one_of(Fac_clothing))),
    Fac_home_furnishing = rowSums(select(., one_of(Fac_home_furnishing))),
    Fac_luxury_sports_goods = rowSums(select(., one_of(Fac_luxury_sports_goods))),
    Fac_diy_workshops = rowSums(select(., one_of(Fac_diy_workshops))),
    Fac_retail_establishments = rowSums(select(., one_of(Fac_retail_establishments))),
    Fac_electronics_appliances = rowSums(select(., one_of(Fac_electronics_appliances))),
    Fac_cosmetics_establishments = rowSums(select(., one_of(Fac_cosmetics_establishments))),
    Fac_adult = rowSums(select(., one_of(Fac_adult))),
    Fac_operations = rowSums(select(., one_of(Fac_operations))),
    Fac_healthcare = rowSums(select(., one_of(Fac_healthcare))),
    Fac_new_collective = rowSums(select(., starts_with("Fac_")[23:ncol(.)]))
  ) %>%
  select(
    coordinateX,
    coordinateY,
    Fac_new1,
    Fac_new2,
    Fac_new_collective,
    Ratings,
    Home,
    Address,
    City
  )

# Get the index of the column 'Ratings'
ratings_index <- which(colnames(df) == "Ratings")

# Select all columns after 'Ratings'
columns_after_ratings <- colnames(df)[(ratings_index + 1):ncol(df)]

# Rearrange the columns
column_order <- c("charger_", "coordinateY", "Fac_sum_1", "Fac_sum_2", columns_after_ratings)
df <- df[, column_order, drop = FALSE]


#### Step 4: Removing those columns that have occurrences in less than five charging stations

#extracting the column names starting with "Fac_"
fac_columns <- grep("^Fac_", names(reshaped_poi_locations), value = TRUE)

#calculating the sum of values in each "Fac_" column
#fac_sums <- sapply(reshaped_poi_locations[, fac_columns], sum) #we actually decide not to do this but rather count occurrences

#counting occurrences of entries larger than 0 in each "Fac_" column
occurrences <- sapply(fac_columns, function(col) sum(reshaped_poi_locations[[col]] > 0))

#identifying the "Fac_" columns with less than 5 occurrences of entries larger than 0
columns_to_remove <- fac_columns[occurrences < 5]

# Remove the selected columns from the dataframe
reshaped_poi_locations <- reshaped_poi_locations %>% select(-all_of(columns_to_remove))




#### Step 5: aggregation (tbd)

#aggregate variables into categories



#figure out if there are unaggregated variables left between the "coordinates" and "rating"



# insignificant_columns <- c()
# for(i in 3:ncol(reshaped_poi_locations)){
#   if(grepl("Fac_", names(reshaped_poi_locations[,i])) == TRUE){
#     if(colSums(abs(reshaped_poi_locations[,i]), na.rm = TRUE) <= 1){
#       insignificant_columns <- c(insignificant_columns, names(reshaped_poi_locations[,i]))
#     }
#     }
# }
# reshaped_poi_locations = reshaped_poi_locations[,-insignificant_columns]
#(reshaped_poi_locations[ , grepl( "Fac_" , names( reshaped_poi_locations ) ) ]){
