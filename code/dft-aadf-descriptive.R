# Aim: generate descriptive statistics and visualisations of the AADF data, building on dft-aadf.R

library(tidyverse)
traffic_aadf = readRDS("traffic-aadf-29092020.Rds")

traffic_cyclable = traffic_aadf %>%
  filter(road_category != "TM",
         road_category != "PM") %>%
  filter(estimation_method == "Counted")

nrow(traffic_cyclable)
# [1] 183884
length(unique(traffic_cyclable$count_point_id))
# [1] 41980

traffic_cyclable %>%
  group_by(year) %>%
  summarise(n = n())
