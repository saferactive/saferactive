# Aim: generate descriptive statistics and visualisations of the AADF data, building on `geographic-data-cleaning.R`
# Takes input from `geographic-data-cleaning.R`

library(tidyverse)

traffic_cyclable = readRDS("traffic_cyclable_clean.Rds")

repeat_points = traffic_cyclable %>%
  filter(year %in% 2010:2019) %>%
  group_by(count_point_id) %>%
  tally() %>%
  filter(n > 1)
traffic_2010 = traffic_cyclable %>%
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
sum(is.na(traffic_change$change_cycles))/nrow(traffic_change)
# [1] 0.0204
traffic_change$change_cycles[is.na(traffic_change$change_cycles)] = 0
sum(is.na(traffic_change$change_cycles))/nrow(traffic_change)
# [1] 0

traffic_change %>%
  group_by(year) %>%
  summarise(change_cycles = weighted.mean(change_cycles, w = mean_cycles, na.rm = TRUE)) %>%
  ggplot(., aes(x = year, y = change_cycles)) +
  geom_line()

summary(traffic_change$change_cycles)
sum(is.na(traffic_change$change_cycles)) / nrow(traffic_change) # 0% nas

traffic_change %>%
  group_by(name) %>%
  mutate(sum_cycles = sum(pedal_cycles)) %>% # sum across all years
  ungroup() %>%
  group_by(year, name) %>%
  summarise(
    change_cycles = weighted.mean(change_cycles, w = mean_cycles), # within each year and LA, we weight the contribution of count points by their mean count
    mean_cycles = mean(mean_cycles),
    n_cycles = n()
            ) %>%
  ggplot(., aes(x = year, y = change_cycles, group = name)) +
  geom_line(aes(alpha = mean_cycles/100)) +
  ylim(c(0,2))

traffic_change_las = traffic_change %>%
  group_by(name) %>%
  mutate(sum_cycles = sum(pedal_cycles)) %>% # sum across all years
  ungroup() %>%
  group_by(year, name, sum_cycles) %>%
  summarise(
    change_cycles = weighted.mean(change_cycles, w = mean_cycles), # within a year and LA, this is weighted by the mean count of each point
    mean_cycles = mean(mean_cycles),
    n_counts = n()
    )
# View(traffic_change_las)

# #weighted mean again - to weight among LAs using sum_cycles if calculating national mean change_cycles
# traffic_change_national = traffic_change_las %>%
#   group_by(year) %>%
#   mutate(weighted_change = weighted.mean(change_cycles, w = sum_cycles))
# # View(traffic_change_weighted)


# Line width relates to the sum of cycle counts across all years within the LA
summary(traffic_change_las)
ggplot(traffic_change_las, aes(x = year, y = change_cycles, group = name)) +
  geom_line(aes(alpha = sum_cycles)) +
  ylim(c(0,2))

# get results relative to 2011
traffic_change_2011 = traffic_change_las %>%
  ungroup() %>%
  mutate(cycling_2011 = case_when(year == 2011 ~ change_cycles)) %>%
  group_by(name) %>%
  mutate(multiplier = change_cycles / mean(cycling_2011, na.rm = TRUE)) %>%
  ungroup()

las_of_interest = c("Leeds", "Derby", "Southampton",
                    "Nottingham", "Birmingham"
                    , "City of London"
                    # , "Waltham Forest", "Hackney"
                    )

traffic_interest = traffic_change_2011 %>%
  filter(name %in% las_of_interest)

# This is Figure 3.7 in Report 2
ggplot(traffic_change_2011, aes(x = year, y = multiplier, group = name)) +
  geom_line(aes(alpha = sum_cycles)) +
  geom_line(
    aes(
      x = year,
      y = multiplier,
      colour = name,
      group = name
    ),
    lwd = 1.2,
    data = traffic_interest
  ) +
  ylim(c(0, 2.5)) +
  scale_x_continuous(breaks = c(2011, 2015, 2019))
ggsave("figures/aadf-counts-la-multipliers.png")

readr::write_csv(traffic_change_2011, "small-output-datasets/traffic_change_2011.csv")

# Create animated map...
counties_uas_gb = readRDS("counties_uas_gb_2019_ubc.Rds")
counties_uas_gb$name = counties_uas_gb$ctyua19nm
counties_joined = right_join(counties_uas_gb, traffic_change_2011)
summary(counties_uas_gb$name %in% traffic_change_2011$name)

summary(traffic_change_2011$name %in% counties_uas_gb$name)

summary({sel = traffic_cyclable$name %in% counties_uas_gb$name})
# Mode   FALSE    TRUE
# logical      47  183837
traffic_cyclable$name[!sel]

library(tmap)
mf = tm_shape(counties_joined) +
  tm_polygons("multiplier") +
  tm_facets(along = "year")
tmap_animation(mf, filename = "la-multipliers.gif")
browseURL("la-multipliers.gif")

# the las with most cycles counted...
traffic_change_2011 %>%
  group_by(name) %>%
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

