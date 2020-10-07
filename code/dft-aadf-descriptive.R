# Aim: generate descriptive statistics and visualisations of the AADF data, building on `geographic-data-cleaning.R`
# Takes input from `geographic-data-cleaning.R`

library(tidyverse)

traffic_cyclable = readRDS("traffic_cyclable_clean.Rds")

repeat_points = traffic_cyclable %>%
  filter(year %in% 2010:2019) %>%
  group_by(count_point_id) %>%
  tally() %>%
  filter(n > 1)
traffic_2010 = traffic_bam %>%
  filter(count_point_id %in% repeat_points$count_point_id) %>%
  filter(year %in% 2010:2019)

dim(traffic_2010)
# [1] 69971    10

# Get relative change in cycle counts  ------------------------------------

summary(traffic_2010$pedal_cycles) # no nas
traffic_change = traffic_2010 %>%
  group_by(count_point_id) %>%
  mutate(mean_cycles = mean(pedal_cycles)) %>%
  ungroup() %>%
  mutate(change_cycles = pedal_cycles/mean_cycles)

sum(is.na(traffic_bam$change_cycles))/nrow(traffic_bam)
# [1] 0 # no NAs - why was it different in the dft-aadf.R script?

traffic_change %>%
  group_by(year) %>%
  summarise(change_cycles = weighted.mean(change_cycles, w = mean_cycles, na.rm = TRUE)) %>%
  ggplot(., aes(x = year, y = change_cycles)) +
  geom_line()

summary(traffic_change$change_cycles)
sum(is.na(traffic_change$change_cycles)) / nrow(traffic_change) # 2% nas

traffic_change_las = traffic_change %>%
  filter(!is.na(change_cycles)) %>%
  mutate(sum_cycles = sum(pedal_cycles)) %>%
  group_by(year, local_authority_name) %>%
  summarise(
    # take the sum not the mean of cycle counters so that LAs with more counters rank higher
    change_cycles = weighted.mean(change_cycles, w = sum_cycles),
    mean_cycles = mean(mean_cycles),
    sum_cycles = sum(mean_cycles)
    )

summary(traffic_change_las)
ggplot(traffic_change_las, aes(x = year, y = change_cycles, group = local_authority_name)) +
  geom_line(aes(alpha = sum_cycles)) +
  ylim(c(0,2))

# get results relative to 2011
traffic_change_2011 = traffic_change_las %>%
  ungroup() %>%
  mutate(cycling_2011 = case_when(year == 2011 ~ change_cycles)) %>%
  group_by(local_authority_name) %>%
  mutate(multiplier = change_cycles / mean(cycling_2011, na.rm = TRUE)) %>%
  ungroup()

las_of_interest = c("Leeds", "Derby", "Southampton",
                    "Nottingham", "Birmingham"
                    , "City of London"
                    # , "Waltham Forest", "Hackney"
                    )

traffic_interest = traffic_change_2011 %>%
  filter(local_authority_name %in% las_of_interest)

ggplot(traffic_change_2011, aes(x = year, y = multiplier, group = local_authority_name)) +
  geom_line(aes(alpha = sum_cycles)) +
  geom_line(
    aes(
      x = year,
      y = multiplier,
      colour = local_authority_name,
      group = local_authority_name
    ),
    lwd = 1.2,
    data = traffic_interest
  ) +
  ylim(c(0, 2)) +
  scale_x_continuous(breaks = c(2011, 2015, 2019))

traffic_change_2011 %>%
  group_by(local_authority_name) %>%
  summarise(sum_cycles = mean(sum_cycles)) %>%
  top_n(n = 5, wt = sum_cycles)

# Geographic descriptive stats --------------------------------------------


traffic_cyclable = readRDS("traffic_cyclable_clean.Rds")
counties_uas_gb = readRDS("counties_uas_gb_2019_ubc.Rds")
counties_uas_gb$name = counties_uas_gb$ctyua19nm

# traffic_aadf_sf = traffic_cyclable %>%
#   sf::st_as_sf(coords = c("easting", "northing"), crs = 27700)
# summary(sf::st_geometry_type(traffic_aadf_sf))

# county aggregation
traffic_aadf_sf_las = traffic_cyclable %>%
  group_by(name) %>%
  summarise_at(vars(pedal_cycles:all_motor_vehicles), .funs = mean)

# geographic plots:
counties_joined = inner_join(counties_uas_gb, traffic_aadf_sf_las)
counties_joined %>%
  select(pedal_cycles) %>%
  mapview::mapview()
counties_joined %>%
  select(pedal_cycles, all_motor_vehicles) %>%
  plot()

# make a map showing level of increase

