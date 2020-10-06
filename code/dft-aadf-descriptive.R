# Aim: generate descriptive statistics and visualisations of the AADF data, building on `geographic-data-cleaning.R`

library(tidyverse)
traffic_cyclable = readRDS("traffic_cyclable_clean.Rds")
counties_uas_gb = readRDS("counties_uas_gb_2019_ubc.Rds")
counties_uas_gb$name = counties_uas_gb$ctyua19nm

# traffic_aadf_sf = traffic_cyclable %>%
#   sf::st_as_sf(coords = c("easting", "northing"), crs = 27700)
# summary(sf::st_geometry_type(traffic_aadf_sf))

# county aggregation
traffic_aadf_sf_las = traffic_cyclable %>%
  group_by(name) %>%
  summarise_at(vars(pedal_cycles:all_motor_vehicles), .funs = mean)

counties_joined = inner_join(counties_uas_gb, traffic_aadf_sf_las)
counties_joined %>%
  select(pedal_cycles) %>%
  mapview::mapview()
counties_joined %>%
  select(pedal_cycles, all_motor_vehicles) %>%
  plot()

# make a map showing level of increase

