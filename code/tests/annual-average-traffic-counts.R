# Aim: get clean geographic data with 2019 county and UA definitions used throughout
# Script also removes counts from motorways and estimated counts
# Output used in `gam-model.R` (raw counts) and `dft-aadf.R` (annual average daily flows)

library(tidyverse)
remotes::install_github("itsleeds/dftTrafficCounts")
library(dftTrafficCounts)
remotes::install_github("Robinlovelace/ukboundaries")
library(ukboundaries)

# For annual average daily flows
# u = "http://data.dft.gov.uk/road-traffic/dft_traffic_counts_aadf.zip"
u = "https://storage.googleapis.com/dft-statistics/road-traffic/downloads/data-gov-uk/dft_traffic_counts_aadf.zip"
d = dtc_import(u = u)

saveRDS(d, "traffic-aadf-14072021.Rds")
piggyback::pb_upload("traffic-aadf-14072021.Rds", tag = "0.1.4")
traffic_aadf = readRDS("traffic-aadf-14072021.Rds")

dim(traffic_aadf)
# [1] 489159   32

# change column names
traffic_renamed = traffic_aadf %>%
  rename(year = AADFYear,
         count_point_id = CP,
         road_category = RoadCategory,
         easting = Easting,
         northing = Northing,
         local_authority_id = LocalAuthority,
         pedal_cycles = PedalCycles,
         all_motor_vehicles = AllMotorVehicles
         )

# DO SPATIAL JOIN INSTEAD
# # lookup Local Authority names
# # For raw counts, join with local authority names
# la_lookup = readRDS("la-lookup.Rds")
# traffic_renamed = traffic_renamed %>%
#   left_join(la_lookup, by = "local_authority_id")
# dim(traffic_renamed)

# problem = traffic_cyclable %>% filter(is.na(local_authority_name))
# unique(problem$local_authority_id)
#
# problem_sf = problem %>% st_as_sf(coords = c("easting", "northing"), crs = 27700)
# problem10 = problem_sf %>% filter(local_authority_id == 210)
# mapview(problem10)
# problem9 = problem_sf %>% filter(local_authority_id == 209)
# mapview(problem9)
#
# problem_sf = problem_sf %>%
#   group_by(count_point_id) %>%
#   sf::st_as_sf(coords = c("easting", "northing"), crs = 27700)
# nrow(traffic_sf)
# # [1] 44789
# summary(sf::st_geometry_type(traffic_sf))
# mapview::mapview(traffic_sf)

# including estimated counts in year that were missed
traffic_cyclable = traffic_renamed %>%
  filter(road_category != "TM",
         road_category != "PM") # %>%
# filter(estimation_method == "Counted") # not needed
# there are some roads with estimation_method_detailed "dependent on a nearby count point". This is where a road crosses a county boundary and the same count has been applied to segments either side of this boundary. These points are included.
dim(traffic_cyclable)
# [1] 465654     33

# Fix points with location errors
error1 = traffic_cyclable %>%
  filter(count_point_id == 946853)
error2 = traffic_cyclable %>%
  filter(count_point_id == 952939)
error3 = traffic_cyclable %>%
  filter(count_point_id == 89108)

error1[error1$easting == 135809,] = error1[error1$easting == 135809,] %>%
  mutate(northing = 24870)
error2 = error2 %>%
  mutate(northing = 221460)
error3 = error3 %>%
  mutate(northing = 218789)

traffic_cyclable = traffic_cyclable %>%
  filter(count_point_id != 952939,
         count_point_id != 946853,
         count_point_id != 89108)
traffic_cyclable = rbind(traffic_cyclable, error1, error2, error3)



# Make map of LAs ---------------------------------------------------------

# download uas/counties - bfc
u = "https://opendata.arcgis.com/datasets/43b324dc1da74f418261378a9a73227f_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
# ultra-generalised - not used
# u = "https://opendata.arcgis.com/datasets/b216b4c8a4e74f6fb692a1785255d777_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
dir.create("counties-uas-2019-bfc")
setwd("counties-uas-2019-bfc/")
counties_gb = ukboundaries::duraz(u)
setwd("..")
getwd() # in the right directory again 🎉
counties_uas_gb = counties_gb %>% filter(!str_detect(string = ctyua19cd, "N"))
saveRDS(counties_uas_gb, "counties_uas_gb_2019_bfc.Rds")
piggyback::pb_upload("counties_uas_gb_2019_bfc.Rds")
piggyback::pb_download_url("counties_uas_gb_2019_ubc.Rds")
# "https://github.com/saferactive/saferactive/releases/download/0.1.1/counties_uas_gb_2019_bfc.Rds"
# "https://github.com/saferactive/saferactive/releases/download/0.1.1/counties_uas_gb_2019_bfc.Rds"

piggyback::pb_download_url("counties_uas_gb_2019_bfc.Rds")
mapview::mapview(counties_uas_gb) # very detailed

traffic_sf = traffic_cyclable %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700)

traffic_joined = sf::st_join(traffic_sf, counties_uas_gb)
nrow(traffic_joined)
# [1] 465654

# spatial join didn't work for count points on bridges. need to find nearest LA for these
missing = traffic_joined %>% filter(
  is.na(ctyua19nm)
)
mapview(missing)

# need to use lapply or something similar. Not working yet.
# then rejoin the results back into traffic_joined
missing$la_name = lapply(missing, FUN = st_nearest_feature(x = missing, y = counties_uas_gb))



saveRDS(traffic_joined, "traffic_joined.Rds")
piggyback::pb_upload("traffic_joined.Rds")
