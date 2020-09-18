

library(tidyverse)
library(mgcv)
library(ggplot2)
# library(viridis)
library(mapview)
# theme_set(theme_bw())
# library(ggpubr)

traffic_national = readRDS("traffic_data_national.Rds")


traffic_points = traffic_national %>%
  select(year, count_date, hour, local_authority_name, count_point_id, easting, northing, pedal_cycles) %>%
  group_by(year, count_date, hour, local_authority_name, count_point_id, easting, northing) %>%
  summarise(pedal_cycles = sum(pedal_cycles)) %>%
  ungroup()

traffic_bam = transform(traffic_points,
                               count_point_id = factor(count_point_id),
                               local_authority_name = factor(local_authority_name),
                               DoY = as.numeric(lubridate::yday(count_date)))

# Assign count points to 2km grid squares # this has to use `round()` instead of `signif()`
traffic_bam = traffic_bam %>%
  mutate(grid_location = paste((round((traffic_bam$easting/2), digits = -3)*2),(round((traffic_bam$northing/2), digits = -3)*2)))

dim(traffic_bam)
length(unique(traffic_bam$grid_location))

traffic_bam %>%
  filter(count_point_id == c(952939, 946853))

#  Investigate count point numbers and placements per/across the year ----------------
traffic_days = traffic_bam %>%
  group_by(year, count_date, DoY, local_authority_name, count_point_id, easting, northing, grid_location) %>%
  summarise(pedal_cycles = sum(pedal_cycles))

#Get London Borough boundaries for maps
lads = readRDS("lads.Rds")

traffic_days %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  # filter(year == 2011) %>%
  mapview() + mapview(lads)

traffic_london_days %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  filter(year == 2018) %>%
  mapview() + mapview(lads)

traffic_london_days %>%
  group_by(year) %>%
  count()
