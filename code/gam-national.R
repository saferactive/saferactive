

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


# Fix points with location errors
error1 = traffic_bam %>%
  filter(count_point_id == 946853)
error2 = traffic_bam %>%
  filter(count_point_id == 952939)

error1[error1$easting == 135809,] = error1[error1$easting == 135809,] %>%
  mutate(northing = 24870)
error2 = error2 %>%
  mutate(northing = 221460)



traffic_bam = traffic_bam %>%
  filter(count_point_id != 952939,
         count_point_id != 946853)
traffic_bam = rbind(traffic_bam, error1, error2)


# Assign count points to 2km grid squares # this has to use `round()` instead of `signif()`
traffic_bam = traffic_bam %>%
  mutate(grid_location = paste((round((traffic_bam$easting/2), digits = -3)*2),(round((traffic_bam$northing/2), digits = -3)*2)))

# dim(traffic_bam)
# length(unique(traffic_bam$grid_location))


#  Investigate count point numbers and placements per/across the year ----------------
traffic_days = traffic_bam %>%
  group_by(year, count_date, DoY, local_authority_name, count_point_id, easting, northing, grid_location) %>%
  summarise(pedal_cycles = sum(pedal_cycles))

#Get LA boundaries for maps
lads = readRDS("lads.Rds")

traffic_days %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  # filter(year == 2011) %>%
  mapview() + mapview(lads)

#number of count points per year
traffic_days %>%
  group_by(year) %>%
  count()
