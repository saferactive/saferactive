# Aim: pre-process network for viz and crash aggregation

remotes::install_github("itsleeds/osmextract")
library(osmextract)
library(dplyr)

library(sf)
# osm_lines = oe_get("Greater London", extra_tags = c("ref", "maxspeed"))
# names(osm_lines)
# waterloo = osm_lines %>%
#   filter(name == "Waterloo Bridge")
#
# waterloo_buffer = waterloo %>%
#   sf::st_union() %>%
#   stplanr::geo_buffer(dist = 1000)
#
# mapview::mapview(waterloo_buffer)
# osm_case_study = osm_lines[waterloo_buffer, , op = sf::st_within]
# table(osm_case_study$highway)
# osm_highways = osm_case_study %>%
#   filter(!is.na(highway))
#
# saveRDS(osm_highways, "osm_highways.Rds")
# piggyback::pb_upload("osm_highways.Rds")
# piggyback::pb_download_url("osm_highways.Rds")

download.file("https://github.com/saferactive/saferactive/releases/download/0.1/osm_highways.Rds", "osm_highways.Rds")

osm_highways = readRDS("osm_highways.Rds")

osm_named_highways = osm_highways %>%
  filter(!is.na(name)) %>%
  group_by(ref, name) %>%
  summarise(
    n = n()
  )

mapview::mapview(osm_named_highways)

oe_download("Greater London", provider = "geofrabrik")


# small dataset
# osmextract::oe_update(place = "Isle of Wight")
# iow_roads = osmextract::oe_get("Isle of Wight")
# city_centre = tmaptools::geocode_OSM()

mapview::mapview(iow_roads[1:100, ])