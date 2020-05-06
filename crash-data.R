# Aim: download data for london for this issue:
#   https://github.com/ITSLeeds/saferroadsmap/issues/26

library(sf)
library(stats19)
library(tidyverse)

years = 2009:2018
crashes_all = get_stats19(year = years, type = "ac")
casualties_all = get_stats19(year = years, type = "cas")
vehicles_all = get_stats19(year = years, type = "veh")


# Joining datasets and getting active casualties -----------------------------------


nrow(crashes_all)
length(unique(crashes_all$accident_index))
# they are all unique
casualties_j = inner_join(crashes_all, casualties_all, by = "accident_index")
nrow(casualties_j) / nrow(crashes_all) # 1.33 casualties per accident on average

# casualties_j2 = left_join(casualties_j, vehicles_all) # this doesn't work (vehicles without casualties excluded)
# nrow(casualties_j2) / nrow(crashes_all) # 1.33

# casualties_j3 = left_join(casualties_j, vehicles_all, by = "accident_index") # doesn't work (vehicles without casualties excluded and vehicles with multiple casualties repeated)
# nrow(casualties_j3) / nrow(crashes_all) # 2.59!


vehicle_casualty_casualties = paste0(casualties_all$accident_index, casualties_all$vehicle_reference)
vehicle_casualty_vehicles = paste0(vehicles_all$accident_index, vehicles_all$vehicle_reference)

# summary(vehicle_casualty_casualties %in% vehicle_casualty_vehicles) # 100% of vehicles associated with casualties are in there
# summary(vehicle_casualty_vehicles %in% vehicle_casualty_casualties) # 38% of vehicles with no associated casualties
# 1004022 / nrow(vehicles_all)

# this selects the vehicles that didn't contain a casualty
vehicles_without_casualty = vehicles_all %>%
  filter(!vehicle_casualty_vehicles %in% vehicle_casualty_casualties)
# nrow(vehicles_without_casualty) / nrow(casualties_j) # 52% smaller than n. casualties


###Seems not to be necessary (joining based on vehicles that didn't contain casualties)
# # This selects the vehicles where only one vehicle in that crash didn't contain a casualty
# single_vehicle_not_in_casualty = vehicles_without_casualty %>%
#   group_by(accident_index) %>%
#   arrange(1:nrow(.)) %>%
#   filter(row_number() == 1)
# nrow(single_vehicle_not_in_casualty) / nrow(casualties_j) # 45% had a single vehicle without a casualty
#
# casualties_j4 = inner_join(casualties_j, single_vehicle_not_in_casualty, by = "accident_index")
# casualties_j5 = left_join(casualties_j, single_vehicle_not_in_casualty, by = "accident_index") # same result
#
#
#
# summary(casualties_j4$vehicle_reference.y) # no NAs
# summary(casualties_j5$vehicle_reference.y) # many NAs
# sum(is.na(casualties_j5$vehicle_reference.y) / nrow(casualties_all)) # 43% have no vehicle data!
# sum(is.na(casualties_j5$vehicle_type) / nrow(casualties_all)) # 43% have no vehicle data!
#
# casualties_active = casualties_j5 %>%
#   filter(casualty_type == "Pedestrian" | casualty_type == "Cyclist")

##

casualties_active = casualties_all %>%
  filter(casualty_type == "Pedestrian" | casualty_type == "Cyclist")

casualties_cycle = casualties_all %>%
  filter(casualty_type == "Cyclist")

casualties_ped = casualties_all %>%
  filter(casualty_type == "Pedestrian")

saveRDS(casualties_active, "casualties_active.Rds")


# Getting a list of vehicle types for each collision --------------------------------

# Simplify vehicle types
vehicles_all_s = vehicles_all
vehicles_all_s$vehicle_type_simple = NULL
vehicles_all_s$vehicle_type_simple[grepl("otorcyc",vehicles_all_s$vehicle_type)] = "Motorcycle"
vehicles_all_s$vehicle_type_simple[grepl("7.5",vehicles_all_s$vehicle_type)] = "HGV"
vehicles_all_s$vehicle_type_simple[grepl("Pedal",vehicles_all_s$vehicle_type)] = "Bicycle"
vehicles_all_s$vehicle_type_simple[vehicles_all_s$vehicle_type == "Car"] = "Car"
vehicles_all_s$vehicle_type_simple[grepl("weight",vehicles_all_s$vehicle_type)] = "OtherGoods"
vehicles_all_s$vehicle_type_simple[grepl("coach",vehicles_all_s$vehicle_type)] = "Bus"
vehicles_all_s$vehicle_type_simple[grepl("Van",vehicles_all_s$vehicle_type)] = "OtherGoods"
vehicles_all_s$vehicle_type_simple[grepl("Taxi",vehicles_all_s$vehicle_type)] = "Taxi"
vehicles_all_s$vehicle_type_simple[is.na(vehicles_all_s$vehicle_type_simple)] = "OtherOrUnknown"


# Join vehicles to crashes using accident index
crash_veh = inner_join(crashes_all, vehicles_all_s, by = "accident_index")

# Find frequency of different types of vehicle
vehicle_freq = table(crash_veh$vehicle_type)
vehicle_freq_simple = table(crash_veh$vehicle_type_simple)

vehicle_freq
vehicle_freq_simple

sum(vehicle_freq)
sum(vehicle_freq_simple)

# Make column that concatenates the (simplified) vehicle types for all vehicles in a crash
short = select(crash_veh,c(accident_index,number_of_vehicles,vehicle_reference,vehicle_type_simple))

longform = pivot_wider(short,id_cols = accident_index,names_from = vehicle_reference, values_from = vehicle_type_simple)

# Concatenate the list of vehicles and remove NAs
unique(short$vehicle_reference)
conc = unite(longform,col = "veh_list","1":"999",na.rm = TRUE)

crashes_all = inner_join(crashes_all, conc, by = "accident_index")

##

crashes_cycle = crashes_all %>%
  filter(accident_index %in% casualties_cycle$accident_index)

crashes_ped = crashes_all %>%
  filter(accident_index %in% casualties_ped$accident_index)

crashes_all$cycle_casualty = ifelse(crashes_all$accident_index %in% crashes_cycle$accident_index, "yes", "no")
crashes_all$ped_casualty = ifelse(crashes_all$accident_index %in% crashes_ped$accident_index, "yes", "no")

crashes_active = crashes_all %>%
  filter(accident_index %in% casualties_active$accident_index)

saveRDS(crashes_active, "crashes_active.Rds")

# Data for London ---------------------------------------------------------




table(casualties_active$police_force)
casualties_active_london = casualties_active %>%
  filter(police_force == "Metropolitan Police" | police_force == "Central") %>%
  stats19::format_sf(lonlat = TRUE)

saveRDS(casualties_active_london, "casualties_active_london.Rds")
piggyback::pb_upload("casualties_active_london.Rds")
# Aim: identify largest vehicle

# Categorisation of crash e.g.:
# 1-car-1-cycling
# 1-car-multi-cycling
# multi-car-1-cycling
# multi-car-multi-cycling
# 1-cycle-pedestrian
# multi-cycling-pedestrian
# multi-cycling-multi-pedestrian
# 1-car-1-pedestrian
# 1-car-multi-pedestrian
# multi-car-1-pedestrian
# multi-car-multi-pedestrian
# 1-cycle-pedestrian
summary(as.factor(casualties_j2$police_force))

casuaties_london = casualties_j2 %>%
  filter(police_force == "Metropolitan Police" | police_force == "City of London")

casuaties_london_active = casuaties_london %>%
  filter(casualty_type == "Pedestrian" | casualty_type == "Cyclist")

casuaties_london_active_sf = format_sf(casuaties_london_active)

plot(casuaties_london_active_sf["casualty_type"])


# Aggregate per borough per year ------------------------------------------

summary(as.factor(casuaties_london_active$local_authority_district))
casuaties_london_active_sf$year = lubridate::year(casuaties_london_active_sf$datetime)
summary(as.factor(casuaties_london_active_sf$year))

saveRDS(casuaties_london_active_sf, "casualties_london_active_sf.Rds")
piggyback::pb_upload("casualties_london_active_sf.Rds")
