# Aim: get traffic calming data from OSM

library(tidyverse)
library(sf)
remotes::install_github("saferactive/traffiCalmr")

d = traffiCalmr::tc_get_osm(bbox = "Greater London")
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
