# Aim: get traffic calming data from OSM

library(tidyverse)
library(sf)
remotes::install_github("saferactive/trafficalmr")

d = trafficalmr::tc_get_osm(bbox = "Greater London")
nrow(d)
plot(d["traffic_calming"])
library(tmap)
tmap_mode("view")
qtm(d, "traffic_calming")
tm_shape(d) + tm_dots("traffic_calming")

table(traffic_calming_osm_london$traffic_calming)
d_new = d %>%
  mutate(
    intervention = case_when(
      traffic_calming == "bump;choker" ~ "bump",
      traffic_calming == "choker;cushion" ~ "cushion",
      traffic_calming == "choker;table" ~ "table",
      traffic_calming == "table;choker" ~ "table",
      grepl(pattern = "rumble|yes", x = traffic_calming) ~ "other",
      TRUE ~ traffic_calming
    )
  )

table(d_new$intervention)

traffic_calming_osm_london = d_new
file.remove("traffic_calming_osm_london.geojson")
sf::st_write(traffic_calming_osm_london, "traffic_calming_osm_london.geojson")
piggyback::pb_upload("traffic_calming_osm_london.geojson")
piggyback::pb_download_url("traffic_calming_osm_london.geojson")

traffic_calming_osm_london = sf::read_sf("https://github.com/saferactive/saferactive/releases/download/0.1/traffic_calming_osm_london.geojson")

lnd = spData::lnd
# names(lnd)
lnd = lnd %>%
  select(Borough = NAME)
traffic_calming_osm_london = sf::st_join(traffic_calming_osm_london, lnd)


tc_london_results = traffic_calming_osm_london %>%
  sf::st_drop_geometry() %>%
  mutate(
    intervention = case_when(
      traffic_calming == "bump;choker" ~ "bump",
      traffic_calming == "choker;cushion" ~ "cushion",
      traffic_calming == "choker;table" ~ "table",
      traffic_calming == "table;choker" ~ "table",
      grepl(pattern = "rumble|yes", x = traffic_calming) ~ "other",
      TRUE ~ traffic_calming
    )
  ) %>%
  group_by(intervention) %>%
  summarise(
    n = n(),
    percent = n/nrow(traffic_calming_osm_london) * 100,
    borough_with_most = Borough[which.max(n)]
    )

readr::write_csv(tc_london_results, "tc_london_results.csv")
piggyback::pb_upload("tc_london_results.csv", repo = "saferactive/saferactive")
piggyback::pb_download_url("tc_london_results.csv")

# from cid

remotes::install_github("PublicHealthDataGeek/CycleInfraLnd")
library(CycleInfraLnd)

cid_traffic_calming = sf::read_sf("https://cycling.data.tfl.gov.uk/CyclingInfrastructure/data/points/traffic_calming.json")

summary(cid_traffic_calming)
head(cid_traffic_calming)
cid_london_results = cid_traffic_calming %>%
  sf::st_drop_geometry() %>%
  group_by(intervention) %>%
  summarise(
    n = n(),
    percent = n/nrow(traffic_calming_osm_london) * 100,
    borough_with_most = Borough[which.max(n)]
  )

readr::write_csv(cid_london_results, "cid_london_results.csv")
piggyback::pb_upload("cid_london_results.csv", repo = "saferactive/saferactive")
piggyback::pb_download_url("cid_london_results.csv")


