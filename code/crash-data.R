library(sf)
library(stats19)
library(tidyverse)

years = 2009:2018
crashes_all = get_stats19(year = years, type = "ac")
casualties_all = get_stats19(year = years, type = "cas")
vehicles_all = get_stats19(year = years, type = "veh")


casualties_active = casualties_all %>%
  filter(casualty_type == "Pedestrian" | casualty_type == "Cyclist")

casualties_cycle = casualties_all %>%
  filter(casualty_type == "Cyclist")

casualties_ped = casualties_all %>%
  filter(casualty_type == "Pedestrian")

saveRDS(casualties_active, "casualties_active.Rds")


# Simplify casualty types and count active travel casualties -----------------------------------


# Simplify casualty types
casualties_all$casualty_type_simple = NULL
casualties_all$casualty_type_simple[grepl("Cyclist",casualties_all$casualty_type)] = "Cyclist"
casualties_all$casualty_type_simple[grepl("Pedestrian",casualties_all$casualty_type)] = "Pedestrian"
casualties_all$casualty_type_simple[grepl("otorcyc",casualties_all$casualty_type)] = "MotorcycleOccupant"
casualties_all$casualty_type_simple[grepl("7.5",casualties_all$casualty_type)] = "HGVOccupant"
casualties_all$casualty_type_simple[casualties_all$casualty_type == "Car occupant"] = "CarOccupant"
casualties_all$casualty_type_simple[grepl("weight",casualties_all$casualty_type)] = "OtherGoodsOccupant"
casualties_all$casualty_type_simple[grepl("coach",casualties_all$casualty_type)] = "BusOccupant"
casualties_all$casualty_type_simple[grepl("Van",casualties_all$casualty_type)] = "OtherGoodsOccupant"
casualties_all$casualty_type_simple[grepl("Taxi",casualties_all$casualty_type)] = "TaxiOccupant"
casualties_all$casualty_type_simple[is.na(casualties_all$casualty_type_simple)] = "OtherOrUnknown"

########## Join casualties to crashes using accident index
crash_cas = inner_join(crashes_all, casualties_all, by = "accident_index")

# # Find frequency of different types of casualty
# cas_freq = table(crash_cas$casualty_type)
# cas_freq_simple = table(crash_cas$casualty_type_simple)
#
# cas_freq
# cas_freq_simple
#
# sum(cas_freq) # 10 casualties have casualty_type NA
# sum(cas_freq_simple)

# Count the number of active casualties in each crash
ped_count = crash_cas %>%
  filter(casualty_type_simple == "Pedestrian") %>%
  select(accident_index, casualty_type_simple) %>%
  group_by(accident_index) %>%
  tally()

cycle_count = crash_cas %>%
  filter(casualty_type_simple == "Cyclist") %>%
  select(accident_index, casualty_type_simple) %>%
  group_by(accident_index) %>%
  tally()


##################

# Getting a list of vehicle types for each collision --------------------------------

# Simplify vehicle types
vehicles_all$vehicle_type_simple = NULL
vehicles_all$vehicle_type_simple[grepl("otorcyc",vehicles_all$vehicle_type)] = "Motorcycle"
vehicles_all$vehicle_type_simple[grepl("7.5",vehicles_all$vehicle_type)] = "HGV"
vehicles_all$vehicle_type_simple[grepl("Pedal",vehicles_all$vehicle_type)] = "Bicycle"
vehicles_all$vehicle_type_simple[vehicles_all$vehicle_type == "Car"] = "Car"
vehicles_all$vehicle_type_simple[grepl("weight",vehicles_all$vehicle_type)] = "OtherGoods"
vehicles_all$vehicle_type_simple[grepl("coach",vehicles_all$vehicle_type)] = "Bus"
vehicles_all$vehicle_type_simple[grepl("Van",vehicles_all$vehicle_type)] = "OtherGoods"
vehicles_all$vehicle_type_simple[grepl("Taxi",vehicles_all$vehicle_type)] = "Taxi"
vehicles_all$vehicle_type_simple[is.na(vehicles_all$vehicle_type_simple)] = "OtherOrUnknown"

# Join vehicles to crashes using accident index
crash_veh = inner_join(crashes_all, vehicles_all, by = "accident_index")

# Find frequency of different types of vehicle
# vehicle_freq = table(crash_veh$vehicle_type)
# vehicle_freq_simple = table(crash_veh$vehicle_type_simple)
#
# vehicle_freq
# vehicle_freq_simple
#
# sum(vehicle_freq)
# sum(vehicle_freq_simple)

# Make column that concatenates the (simplified) vehicle types for all vehicles in a crash
short = select(crash_veh,c(accident_index,number_of_vehicles,vehicle_reference,vehicle_type_simple))

longform = pivot_wider(short,id_cols = accident_index,names_from = vehicle_reference, values_from = vehicle_type_simple)

# Concatenate the list of vehicles and remove NAs
# unique(short$vehicle_reference)
conc = unite(longform,col = "veh_list","1":"999",na.rm = TRUE)

crashes_all = inner_join(crashes_all, conc, by = "accident_index")

##########

crashes_cycle = crashes_all %>%
  filter(accident_index %in% casualties_cycle$accident_index)

crashes_ped = crashes_all %>%
  filter(accident_index %in% casualties_ped$accident_index)

crashes_all$cycle_casualty = ifelse(crashes_all$accident_index %in% crashes_cycle$accident_index, "yes", "no")
crashes_all$ped_casualty = ifelse(crashes_all$accident_index %in% crashes_ped$accident_index, "yes", "no")

crashes_all = crashes_all %>%
  left_join(ped_count, by = "accident_index") %>%
  rename(number_ped_casualties = n)

crashes_all = crashes_all %>%
  left_join(cycle_count, by = "accident_index") %>%
  rename(number_cycle_casualties = n)

saveRDS(crashes_all, "crashes_all.Rds")

crashes_active_casualties = crashes_all %>%
  filter(accident_index %in% casualties_active$accident_index)



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
