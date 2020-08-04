library(tidyverse)
library(sf)

u = "http://data.dft.gov.uk/road-traffic/dft_traffic_counts_raw_counts.zip"
download.file(u, "./dft_traffic_counts_raw_counts-2000-2018.zip")
traffic_data_original = readr::read_csv("./dft_traffic_counts_raw_counts-2000-2018.zip")

names(traffic_data_original)

counter_16598 = traffic_data_original %>%
  filter(count_point_id == "16598")

counter_16598 %>%
  filter(year == 2016) %>%
  summarise(pedal_cycles = sum(pedal_cycles)) # 326 why not 366???
counter_16598$link_length_km

##London Boroughs

lads = readRDS("lads.Rds")
boroughs = as.character(spData::lnd$NAME)
lads = lads %>%
  filter(Name %in% boroughs)

##Counters per London Borough
counters_la = readr::read_csv("http://data.dft.gov.uk/road-traffic/local_authority_traffic.csv")
counters_lnd = counters_la %>%
  filter(name %in% boroughs)
dim(counters_lnd)

# lads = ukboundaries::lad2018
# summary(lads$Name %in% counters_la$name)
# summary(lads$lau118cd %in% counters_la$ons_code)
# length(unique(counters_la$name))
# length(unique(lads$lau118nm))

## All traffic count data for London
traffic_london = traffic_data_original %>%
  filter(local_authority_name %in% boroughs)

traffic_london = traffic_london %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700)

traffic_london_points = traffic_london %>%
  group_by(count_point_id, direction_of_travel, year) %>%
  mutate(pedal_cycle_km = pedal_cycles * link_length_km,
         pedal_cycles = sum(pedal_cycles))

###Select peak hours only

# ttop = top_frac(traffic_london, 0.1, wt = pedal_cycles)
# mapview::mapview(ttop)

## Aggregate by Borough
traffic_boroughs = traffic_london_points %>%
  group_by(local_authority_name, year) %>%
  summarise(pedal_cycles = mean(pedal_cycles)) %>%
  st_drop_geometry

traffic_boroughs = inner_join(lads, traffic_boroughs, by = c("Name" = "local_authority_name"))

mapview::mapview(traffic_boroughs["pedal_cycles"])
