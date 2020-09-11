library(sf)
library(stats19)
library(tidyverse)

years = 2009:2018
crashes_all = get_stats19(year = years, type = "ac")
casualties_all = readRDS("casualties_adjusted.Rds")
vehicles_all = get_stats19(year = years, type = "veh")

casualties_active = casualties_all %>%
  filter(casualty_type == "Pedestrian" | casualty_type == "Cyclist")

casualties_cycle = casualties_all %>%
  filter(casualty_type == "Cyclist")

casualties_ped = casualties_all %>%
  filter(casualty_type == "Pedestrian")

# saveRDS(casualties_active, "casualties_active.Rds")

# Simplify casualty types and count active travel casualties -----------------------------------

# Simplify casualty types
casualties_all$casualty_type_simple = NULL
casualties_all$casualty_type_simple[grepl("Cyclist",casualties_all$casualty_type)] = "Cyclist"
casualties_all$casualty_type_simple[grepl("Pedestrian",casualties_all$casualty_type)] = "Pedestrian"
casualties_all$casualty_type_simple[grepl("otorcyc",casualties_all$casualty_type)] = "MotorcycleOccupant"
casualties_all$casualty_type_simple[grepl("7.5",casualties_all$casualty_type)] = "HGVOccupant"
casualties_all$casualty_type_simple[casualties_all$casualty_type == "Car occupant"] = "CarOccupant"
casualties_all$casualty_type_simple[grepl("weight",casualties_all$casualty_type)] = "HGVOccupant"
casualties_all$casualty_type_simple[grepl("coach",casualties_all$casualty_type)] = "BusOccupant"
casualties_all$casualty_type_simple[grepl("Van",casualties_all$casualty_type)] = "VanOccupant"
casualties_all$casualty_type_simple[grepl("Taxi",casualties_all$casualty_type)] = "TaxiOccupant"
casualties_all$casualty_type_simple[is.na(casualties_all$casualty_type_simple)] = "OtherOrUnknown"

casualty_types = casualties_all %>%
  group_by(casualty_type, casualty_type_simple) %>%
  tally()

cas_table = casualty_types %>%
  rename(number_of_casualties = n) %>%
  arrange(casualty_type_simple)

write_csv(cas_table, "cas_table.csv")

########## Join casualties to crashes using accident index
crash_cas = inner_join(crashes_all, casualties_all, by = "accident_index")

crash_cas = crash_cas %>%
  mutate_at(vars(Adjusted_Serious:Adjusted_Slight), ~replace(., is.na(.), 0))

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
# Adapted to include fatal casualties, and to base figures on casualty severity, not accident severity
ped_count_f = crash_cas %>%
  filter(casualty_type_simple == "Pedestrian",
         casualty_severity == "Fatal") %>%
  select(accident_index) %>%
  group_by(accident_index) %>%
  tally()

ped_count2 = crash_cas %>%
  filter(casualty_type_simple == "Pedestrian") %>%
  select(accident_index, Adjusted_Serious, Adjusted_Slight) %>%
  group_by(accident_index) %>%
  summarise(Adjusted_Serious = sum(Adjusted_Serious),
            Adjusted_Slight = sum(Adjusted_Slight))

ped_count = right_join(ped_count_f, ped_count2) %>%
  mutate_at(vars(n), ~replace(., is.na(.), 0)) %>%
  rename(ped_fatal = n)

f = sum(ped_count$ped_fatal) #1064
se = sum(ped_count$Adjusted_Serious) #41422.94
sl = sum(ped_count$Adjusted_Slight) #143961.1
se + sl #242373
f + se + sl #246777

# dim(ped_count) # 238538
# length(unique(ped_count$accident_index))


# cycle_count = crash_cas %>%
#   filter(casualty_type_simple == "Cyclist") %>%
#   select(accident_index, casualty_type_simple) %>%
#   group_by(accident_index) %>%
#   tally()
#
# sum(cycle_count$n) #186472

cycle_count_f = crash_cas %>%
  filter(casualty_type_simple == "Cyclist",
         casualty_severity == "Fatal") %>%
  select(accident_index) %>%
  group_by(accident_index) %>%
  tally()

# sum(cycle_count_f$n) #1064

# b = crash_cas %>%
#   filter(casualty_type_simple == "Cyclist")
# sum(b$Adjusted_Serious) #41434.62
# sum(b$Adjusted_Slight) #143973.4
# 41434.64 + 143973.4 #185408

cycle_count2 = crash_cas %>%
  filter(casualty_type_simple == "Cyclist") %>%
  select(accident_index, Adjusted_Serious, Adjusted_Slight) %>%
  group_by(accident_index) %>%
  summarise(Adjusted_Serious = sum(Adjusted_Serious),
            Adjusted_Slight = sum(Adjusted_Slight))

# sum(cycle_count2$Adjusted_Serious) #41434.64
# sum(cycle_count2$Adjusted_Slight) #143973.4

cycle_count = right_join(cycle_count_f, cycle_count2) %>%
  mutate_at(vars(n), ~replace(., is.na(.), 0)) %>%
  rename(cycle_fatal = n)

# f = sum(cycle_count$cycle_fatal) #1064
# se = sum(cycle_count$Adjusted_Serious) #41422.94
# sl = sum(cycle_count$Adjusted_Slight) #143961.1
# se + sl #185408
# f + se + sl #186472

# this result is lower than the original cycle_count tally, it seems to be missing some casualties

# Get a list of vehicle types for each collision --------------------------------

# Simplify vehicle types
vehicles_all$vehicle_type_simple = NULL
vehicles_all$vehicle_type_simple[grepl("otorcyc",vehicles_all$vehicle_type)] = "Motorcycle"
vehicles_all$vehicle_type_simple[grepl("7.5",vehicles_all$vehicle_type)] = "HGV"
vehicles_all$vehicle_type_simple[grepl("Pedal",vehicles_all$vehicle_type)] = "Bicycle"
vehicles_all$vehicle_type_simple[vehicles_all$vehicle_type == "Car"] = "Car"
vehicles_all$vehicle_type_simple[grepl("weight",vehicles_all$vehicle_type)] = "HGV"
vehicles_all$vehicle_type_simple[grepl("coach",vehicles_all$vehicle_type)] = "Bus"
vehicles_all$vehicle_type_simple[grepl("Van",vehicles_all$vehicle_type)] = "Van"
vehicles_all$vehicle_type_simple[grepl("Taxi",vehicles_all$vehicle_type)] = "Taxi"
vehicles_all$vehicle_type_simple[is.na(vehicles_all$vehicle_type_simple)] = "OtherOrUnknown"

# Table of vehicle types
vehicle_types = vehicles_all %>%
  group_by(vehicle_type, vehicle_type_simple) %>%
  tally()

veh_table = vehicle_types %>%
  rename(number_of_vehicles = n) %>%
  arrange(vehicle_type_simple)

write_csv(veh_table, "veh_table.csv")

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

crashes_joined = inner_join(crashes_all, conc, by = "accident_index")

# Join with casualty count data -------------------------------------------

# crashes_cycle = crashes_joined %>%
#   filter(accident_index %in% casualties_cycle$accident_index)
#
# crashes_ped = crashes_joined %>%
#   filter(accident_index %in% casualties_ped$accident_index)
#
# crashes_joined$cycle_casualty = ifelse(crashes_joined$accident_index %in% crashes_cycle$accident_index, "yes", "no")
# crashes_joined$ped_casualty = ifelse(crashes_joined$accident_index %in% crashes_ped$accident_index, "yes", "no")

crashes_joined = crashes_joined %>%
  left_join(cycle_count, by = "accident_index") %>%
  rename(cycle_adjusted_serious = Adjusted_Serious,
         cycle_adjusted_slight = Adjusted_Slight)

crashes_joined = crashes_joined %>%
  left_join(ped_count, by = "accident_index") %>%
  rename(ped_adjusted_serious = Adjusted_Serious,
         ped_adjusted_slight = Adjusted_Slight) %>%
  mutate_at(vars(cycle_fatal:ped_adjusted_slight), ~replace(., is.na(.), 0))

saveRDS(crashes_joined, "crashes_joined.Rds")

# This only includes the crashes containing a cyclist or pedestrian casualty.
crashes_active_casualties = crashes_joined %>%
  filter(accident_index %in% casualties_active$accident_index)

# There are also some crashes involving cyclists where the cyclist is not listed as a casualty (and not all of these have pedestrian casualties either)
crashes_bicycle = crashes_joined %>%
  filter(
    grepl('Bicycle', veh_list))

crashes_bicycle_injured = crashes_joined %>%
  filter(
    grepl('Bicycle', veh_list),
    (cycle_fatal > 0 | cycle_adjusted_serious > 0 | cycle_adjusted_slight >0))

crashes_bicycle_uninjured = crashes_joined %>%
  filter(
    grepl('Bicycle', veh_list),
    (cycle_fatal == 0 & cycle_adjusted_serious == 0 & cycle_adjusted_slight == 0))

# Data for London ---------------------------------------------------------

table(crashes_active_casualties$police_force)

crashes_active_london = crashes_active_casualties %>%
  filter(police_force == "Metropolitan Police" | police_force == "City of London") %>%
  stats19::format_sf(lonlat = TRUE)

# #Remove unneeded columns
# crashes_active_london = crashes_active_london %>%
#   select(c(1,4:25,31:37))

# library(stats19)
# crashes_active_london_sf = format_sf(crashes_active_london)

plot(crashes_active_london["cycle_adjusted_serious"])

# Aggregate per borough per year ------------------------------------------
crashes_active_london$year = lubridate::year(crashes_active_london$datetime)

# summary(as.factor(crashes_active_london$local_authority_district))
# summary(as.factor(crashes_active_london$year))



# Find the combinations of vehicles ---------------------------------------

crashes_active_london = crashes_active_london %>%
  mutate(number_bicycles = str_count(crashes_active_london$veh_list, "Bicycle"),
         number_buses = str_count(crashes_active_london$veh_list, "Bus"),
         number_cars = str_count(crashes_active_london$veh_list, "Car"),
         number_HGVs = str_count(crashes_active_london$veh_list, "HGV"),
         number_motorcycles = str_count(crashes_active_london$veh_list, "Motorcycle"),
         number_vans = str_count(crashes_active_london$veh_list, "Van"),
         number_otherunknown = str_count(crashes_active_london$veh_list, "OtherOrUnknown"),
         number_taxis = str_count(crashes_active_london$veh_list, "Taxi")
  )

cycle_veh_freqs = crashes_active_london %>%
  filter(accident_index %in% casualties_cycle$accident_index) %>%
  select(c(number_bicycles:number_taxis)) %>%
  st_drop_geometry() %>%
  colSums()

ped_veh_freqs = crashes_active_london %>%
  filter(accident_index %in% casualties_ped$accident_index) %>%
  select(c(number_bicycles:number_taxis)) %>%
  st_drop_geometry() %>%
  colSums()

saveRDS(crashes_active_london, "crashes_active_london.Rds")
piggyback::pb_upload("crashes_active_london.Rds")
sf::write_sf(crashes_active_london, "crashes_active_london.geojson")
piggyback::pb_upload("crashes_active_london.geojson")
