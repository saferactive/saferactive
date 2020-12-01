# Aim: get clean geographic data with 2019 county and UA definitions used throughout
# Script also removes counts from motorways and estimated counts
# Output used in `dft-aadf.R` and `dft-aadf-descriptive.R`

library(tidyverse)
remotes::install_github("itsleeds/dftTrafficCounts")
library(dftTrafficCounts)

# For annual average daily flows
u = "http://data.dft.gov.uk/road-traffic/dft_traffic_counts_aadf.zip"
d = dtc_import(u = u)

saveRDS(d, "traffic-aadf-29092020.Rds")
piggyback::pb_upload("traffic-aadf-29092020.Rds", tag = "0.1")
traffic_aadf = readRDS("traffic-aadf-29092020.Rds")

# For raw hourly data (eg for peak hours only)
u = "http://data.dft.gov.uk/road-traffic/dft_traffic_counts_raw_counts.zip"
d = dtc_import(u = u)

saveRDS(d, "traffic-data-29092020.Rds")
piggyback::pb_upload("traffic-data-29092020.Rds", tag = "0.1")
traffic_raw = readRDS("traffic-data-29092020.Rds")

dim(traffic_raw)
# [1] 461948     33
# [1] 4511532      33

names(traffic_raw)
table(traffic_raw$sequence)

# For raw counts, change column names, to make them equivalent to AADF column names
traffic_raw = traffic_raw %>%
  rename(year = Year,
         count_point_id = CP,
         direction_of_travel = iDir,
         pedal_cycles = PedalCycles,
         easting = Easting,
         northing = Northing,
         local_authority_id = LocalAuthority,
         road_category = RoadCategory,
         road_type = RoadType,
         all_motor_vehicles = AllMotorVehicles)

# For raw counts, join with local authority names
la_lookup = readRDS("la-lookup.Rds")
traffic_raw = traffic_raw %>%
  left_join(la_lookup, by = "local_authority_id")
dim(traffic_raw)



# including estimated counts in year that were missed
traffic_cyclable = traffic_raw %>%
  filter(road_category != "TM",
         road_category != "PM") # %>%
  # filter(estimation_method == "Counted") # not needed for raw data
# there are some roads with estimation_method_detailed "dependent on a nearby count point". This is where a road crosses a county boundary and the same count has been applied to segments either side of this boundary. These points are included.

# Fix points with location errors
error1 = traffic_cyclable %>%
  filter(count_point_id == 946853)
error2 = traffic_cyclable %>%
  filter(count_point_id == 952939)

error1[error1$easting == 135809,] = error1[error1$easting == 135809,] %>%
  mutate(northing = 24870)
error2 = error2 %>%
  mutate(northing = 221460)

traffic_cyclable = traffic_cyclable %>%
  filter(count_point_id != 952939,
         count_point_id != 946853)
traffic_cyclable = rbind(traffic_cyclable, error1, error2)

traffic_cyclable$local_authority_name[traffic_cyclable$local_authority_name == "Argyll & Bute"] = "Argyll and Bute"
traffic_cyclable$local_authority_name[traffic_cyclable$local_authority_name == "Bournemouth"] = "Bournemouth, Christchurch and Poole"
traffic_cyclable$local_authority_name[traffic_cyclable$local_authority_name == "Comhairle nan Eilean Siar"] = "Na h-Eileanan Siar"
traffic_cyclable$local_authority_name[traffic_cyclable$local_authority_name == "Cornwall excluding Isles of Scilly"] = "Cornwall"
traffic_cyclable$local_authority_name[traffic_cyclable$local_authority_name == "Dumfries & Galloway"] = "Dumfries and Galloway"
traffic_cyclable$local_authority_name[traffic_cyclable$local_authority_name == "Durham"] = "County Durham"
traffic_cyclable$local_authority_name[traffic_cyclable$local_authority_name == "East Cheshire"] = "Cheshire East"
# traffic_cyclable$local_authority_name[traffic_cyclable$local_authority_name == "Isles of Scilly"] = "Cornwall"
traffic_cyclable$local_authority_name[traffic_cyclable$local_authority_name == "Perth & Kinross"] = "Perth and Kinross"
traffic_cyclable$local_authority_name[traffic_cyclable$local_authority_name == "Poole"] = "Bournemouth, Christchurch and Poole"
traffic_cyclable$local_authority_name[traffic_cyclable$local_authority_name == "Rhondda, Cynon, Taff"] = "Rhondda Cynon Taf"
traffic_cyclable$local_authority_name[traffic_cyclable$local_authority_name == "West Cheshire"] = "Cheshire West and Chester"
traffic_cyclable$local_authority_name[traffic_cyclable$local_authority_name == "The Vale of Glamorgan"] = "Vale of Glamorgan"
traffic_cyclable$local_authority_name[traffic_cyclable$count_point_id == 943953] = "Cheshire East"


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




traffic_raw_sf = traffic_cyclable %>%
  group_by(count_point_id, local_authority_name) %>%
  mutate(easting = mean(easting), northing = mean(northing)) %>%
  group_by(count_point_id, local_authority_name, easting, northing) %>%
  summarise_at(vars(pedal_cycles:all_motor_vehicles), .funs = mean) %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700)
nrow(traffic_raw_sf)
# [1] 42543 ([1] 41980 when local_authority_name is omitted) 44359 with estimated counts
# [1] 41096 for raw counts #41366
summary(sf::st_geometry_type(traffic_raw_sf))
mapview::mapview(traffic_raw_sf)

# county aggregation
traffic_raw_sf_las = traffic_cyclable %>%
  group_by(local_authority_name) %>%
  mutate(easting = mean(easting), northing = mean(northing)) %>%
  group_by(local_authority_name, easting, northing) %>%
  summarise_at(vars(pedal_cycles:all_motor_vehicles), .funs = mean) %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700)
nrow(traffic_raw_sf_las)
# [1] 208
mapview::mapview(traffic_raw_sf_las)
traffic_raw_sf_las_joined = sf::st_join(
  traffic_raw_sf_las,
  counties_uas_gb %>% select(name = ctyua19nm)
)
raw_la_county_lookup = traffic_raw_sf_las_joined %>%
  select(name, local_authority_name) %>%
  filter(name != local_authority_name) %>%
  sf::st_drop_geometry()
readr::write_csv(raw_la_county_lookup, "small-output-datasets/raw_la_county_lookup.csv")
# readr::write_csv(aadf_la_county_lookup, "small-output-datasets/aadf_la_county_lookup.csv")

# join and create lookup at counter id level
traffic_raw_sf_las_joined = sf::st_join(
  traffic_raw_sf,
  counties_uas_gb %>% select(name = ctyua19nm)
)

raw_la_county_lookup_point = traffic_raw_sf_las_joined %>%
  select(count_point_id, name, local_authority_name) %>%
  filter(name != local_authority_name)

raw_la_county_lookup_point
# identify edge case points in aggregated data:
# raw_la_county_lookup_point %>%
#   filter(name == "Swansea" & local_authority_name == "Carmarthenshire") %>%
#   mapview::mapview() +
#   mapview::mapview(counties_uas_gb)

raw_la_county_lookup_point_n = raw_la_county_lookup_point %>%
  sf::st_drop_geometry() %>%
  group_by(name, local_authority_name) %>%
  summarise(n = n()) %>%
  arrange(n)
table(raw_la_county_lookup_point_n$n)
# 1   2   3   4   5  14  59  63  78  79 106 112 125 145 155 181 204 240 264 304 344 409
# 95  18   2   4   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
View(raw_la_county_lookup_point_n)
raw_la_county_lookup_point_n_filtered = raw_la_county_lookup_point_n %>%
  filter(n >= 13)
raw_la_county_lookup_point_n_filtered

traffic_aadf_yrs_la_summary = read_csv("small-output-datasets/traffic_aadf_yrs_la_summary.csv")

summary(sel <- counties_uas_gb$ctyua19nm %in% traffic_aadf_yrs_la_summary$local_authority_name)
counties_gb$ctyua19nm[!sel]

raw_la_county_lookup_point2 = raw_la_county_lookup_point %>%
  sf::st_drop_geometry() %>%
  inner_join(., raw_la_county_lookup_point_n_filtered) %>%
  select(-n)
# readr::write_csv(aadf_la_county_lookup_point2, "small-output-datasets/aadf_la_county_lookup.csv")
readr::write_csv(raw_la_county_lookup_point2, "small-output-datasets/raw_la_county_lookup.csv")

corrections = raw_la_county_lookup_point2 %>% select(count_point_id, name)
remove = duplicated(corrections)
corrections2 = corrections[remove == FALSE,]

# length(unique(corrections2$count_point_id))
# dim(corrections2)
# corrections2 %>%
#   group_by(count_point_id) %>%
#   mutate(n = n()) %>%
#   filter(n > 1)
# traffic_bam %>% filter(count_point_id == 943953) %>%
#   sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>% mapview()



traffic_cyclable_clean = traffic_cyclable %>%
  left_join(., corrections2)
dim(traffic_cyclable_clean) #183884 #439688 #4315332 for raw
summary(as.factor(traffic_cyclable_clean$name))
summary(as.factor(traffic_cyclable_clean$local_authority_name))
traffic_cyclable_clean$name[is.na(traffic_cyclable_clean$name)] =
  traffic_cyclable_clean$local_authority_name[is.na(traffic_cyclable_clean$name)]
summary(traffic_cyclable_clean$name == traffic_cyclable_clean$local_authority_name)
# Mode   FALSE    TRUE    NA's
# logical   2021  180675    1188
traffic_cyclable_clean_no_la = traffic_cyclable_clean %>%
  filter(is.na(name))
traffic_cyclable_clean_no_la_joined = traffic_cyclable_clean_no_la %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  select(count_point_id) %>%
  sf::st_join(., counties_uas_gb %>% select(name_updated = ctyua19nm)) %>%
  sf::st_drop_geometry()

table(traffic_cyclable_clean_no_la_joined$name_updated)
# Glasgow City North Lanarkshire South Lanarkshire
# 611               562                 1

traffic_cyclable_clean = traffic_cyclable_clean %>%
  left_join(., traffic_cyclable_clean_no_la_joined)
summary(as.factor(traffic_cyclable_clean$name_updated))
summary(as.factor(traffic_cyclable_clean$name))

traffic_cyclable_clean$name[is.na(traffic_cyclable_clean$name)] =
  traffic_cyclable_clean$name_updated[is.na(traffic_cyclable_clean$name)]
summary(as.factor(traffic_cyclable_clean$name))
traffic_cyclable_clean %>%
  filter(is.na(name)) %>%
  select(count_point_id, name, easting) %>%
  count(count_point_id)

# where is it?
missing_count_point = traffic_raw_sf %>%
  filter(count_point_id == 50974)
missing_count_point2 = traffic_raw_sf %>%
  filter(count_point_id == 10820)
missing_count_point3 = traffic_raw_sf %>%
  filter(count_point_id == 50728)
missing_count_point4 = traffic_raw_sf %>%
  filter(count_point_id == 82090)
missing_count_point5 = traffic_raw_sf %>%
  filter(count_point_id == 940855)

mapview::mapview(missing_count_point)
mapview::mapview(missing_count_point2)
mapview::mapview(missing_count_point3)
mapview::mapview(missing_count_point4)
mapview::mapview(missing_count_point5)

traffic_cyclable_clean$name[traffic_cyclable_clean$count_point_id == 50974] =
  "Glasgow City"
traffic_cyclable_clean$name[traffic_cyclable_clean$count_point_id == 10820] =
  "Glasgow City"
traffic_cyclable_clean$name[traffic_cyclable_clean$count_point_id == 50728] =
  "Glasgow City"
traffic_cyclable_clean$name[traffic_cyclable_clean$count_point_id == 82090] =
  "Glasgow City"

traffic_cyclable_clean$name[traffic_cyclable_clean$count_point_id == 940855] =
  "Cambridgeshire"

# #remove old local authority name fields
# traffic_cyclable_clean$local_authority_name = NULL
# traffic_cyclable_clean$name_updated = NULL


#remove duplicate rows to prevent error in Glasgow/Lanarkshire NAs
remove = duplicated(traffic_cyclable_clean)
sum(remove) #10618 #68656
traffic_cyclable_clean = traffic_cyclable_clean[remove == FALSE,]
dim(traffic_cyclable_clean) #183884 #439688 #4321788 raw

# saveRDS(traffic_cyclable_clean, "traffic_cyclable_clean.Rds")
saveRDS(traffic_cyclable_clean, "traffic_cyclable_clean_raw.Rds")
piggyback::pb_upload("traffic_cyclable_clean.Rds")

# test code ---------------------------------------------------------------

# # check nrow of different county/ua datasets
# # rapid regions?
# u = "https://opendata.arcgis.com/datasets/54a0620552824e32af97d476b83ca18d_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
# dir.create("counties-uas-2011")
# setwd("counties-uas-2011")
# counties_gb = ukboundaries::duraz(u)
# nrow(counties_gb)
# # [1] 174
# setwd("..")
# getwd() # in the right directory again 🎉
# summary(sel <- counties_gb$ctyua19nm %in% traffic_raw_yrs_la_summary$local_authority_name)
# summary(traffic_raw_yrs_la_summary$local_authority_name %in% counties_gb$ctyua19nm)
# lads_not_in_raw1 = counties_gb[!sel, ]
# mapview::mapview(lads_not_in_raw1)


# dir.create("counties-2019")
# setwd("counties-2019")
# counties_gb = ukboundaries::duraz("https://opendata.arcgis.com/datasets/7a7c4e834b4c493d9c011b4f3d144698_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D")
# mapview::mapview(counties_gb) # missing loads!
# setwd("..")
#
#
#
# traffic_cyclable_years = traffic_cyclable_2 %>%
#   group_by(latitude, longitude) %>%
#   mutate(
#     present_2009 = year %in% 2009,
#     present_2010 = year %in% 2010,
#     present_2011 = year %in% 2011,
#     present_2012 = year %in% 2012,
#     present_2013 = year %in% 2013,
#     present_2014 = year %in% 2014,
#     present_2015 = year %in% 2015,
#     present_2016 = year %in% 2016,
#     present_2017 = year %in% 2017,
#     present_2018 = year %in% 2018,
#     present_2019 = year %in% 2019
#     )
# nrow(traffic_cyclable_years)
# # [1] 166680
# traffic_cyclable_years %>%
#   filter(year == 2009) %>%
#   select(year, present_2009:present_2019)
#
# summary(traffic_cyclable_years %>% select(present_2009:present_2019))
#
# summary_present_years = traffic_cyclable_years %>%
#   group_by(year) %>%
#   summarise(
#     n = n(),
#     present_2009 = sum(present_2009),
#     present_2010 = sum(present_2010),
#     present_2011 = sum(present_2011),
#     present_2012 = sum(present_2012)
#     )
# ggplot(summary_present_years) +
#   geom_line(aes(year, n, col = present_2009))

# lads = readRDS("lads.Rds")
# nrow(lads)
# length(unique(traffic_raw$local_authority_name))
# summary(sel <- lads$Name %in% traffic_raw_yrs_la_summary$local_authority_name)
# # Mode   FALSE    TRUE
# # logical     219     163
# summary(traffic_raw_yrs_la_summary$local_authority_name %in% lads$Name)
# # Mode   FALSE    TRUE
# # logical     380    1625
# lads_in_raw1 = lads[sel, ]
# mapview::mapview(lads_in_raw1)

# # try 2018 definition of uas/counties
# u = "https://opendata.arcgis.com/datasets/d13feea979be44eb83bceeed94a1510d_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
# dir.create("counties-uas-2018")
# setwd("counties-uas-2018")
# counties_gb = ukboundaries::duraz(u)
# setwd("..")
# saveRDS(counties_gb, "counties-uas-2018.Rds")
# getwd() # in the right directory again 🎉
# counties_gb = counties_gb %>% filter(!str_detect(string = cauth18cd, "N"))
# nrow(counties_gb)
# mapview::mapview(counties_gb) # missing loads!
# nrow(counties_gb)
