

library(tidyverse)
library(mgcv)
library(ggplot2)
library(viridis)
library(mapview)
theme_set(theme_bw())
library(ggpubr)

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

# Assign count points to 1km grid squares
traffic_bam = traffic_bam %>%
  mutate(grid_location = paste(signif(traffic_bam$easting, digits = 3),signif(traffic_bam$northing, digits = 3)))

dim(traffic_bam)
length(unique(traffic_bam$grid_location))
