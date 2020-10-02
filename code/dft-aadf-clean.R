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

summary_n_year = traffic_cyclable %>%
  group_by(year) %>%
  summarise(
    n = n(),
    n_ids = length(unique(count_point_id))
    )
summary(summary_n_year$n == summary_n_year$n_ids)
# Mode    TRUE
# logical      20

ggplot(summary_n_year) +
  geom_line(aes(year, n))
ggsave("figures/aadf-counts-per-year.png")

summary_n_year_id = traffic_cyclable %>%
  group_by(year, count_point_id) %>%
  summarise(
    n = n()
  )

nrow(summary_n_year_id) == nrow(traffic_cyclable)
# [1] TRUE # all variability captured in year and counter id

summary_n_id = traffic_cyclable %>%
  count(count_point_id)
table(summary_n_id$n)

summary_n_id %>%
  ggplot() +
  geom_histogram(aes(n), binwidth = 1)
ggsave("figures/aadf-n-repeats-ids.png")
ids_2_plus = summary_n_id %>% filter(n > 1) %>% pull(count_point_id)

traffic_cyclable_2 = traffic_cyclable %>%
  filter(count_point_id %in% ids_2_plus)
nrow(traffic_cyclable_2)
traffic_cyclable_2 %>%
  count(count_point_id) %>%
  ggplot() +
  geom_histogram(aes(n), binwidth = 1)

# traffic_yr_combinations = traffic_cyclable_2 %>%
#   group_by(count_point_id) %>%
#   mutate(
#     n_years = length(unique(year)),
#     years = paste0(year, collapse = ","),
#     min_year = min(year),
#     max_year = max(year)
#     ) %>%
#   ungroup()
#
# summary(traffic_yr_combinations$n_years)
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# # 2.000   6.000   9.000   8.632  11.000  20.000
# tyrs = table(traffic_yr_combinations$years)
# tail(tyrs[order(tyrs)])
# # 2000,2001,2002,2003,2004,2006,2007,2008,2009           2000,2001,2002,2003,2004,2005,2006,2008,2009
# # 1323                                                   3240
# # 2003,2004,2005,2006,2007,2008,2009      2000,2001,2002,2003,2004,2005,2006,2007,2008,2009
# # 9667                                                  12370
# # 2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019 2008,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019
# # 14564                                                  17314
#
# summary_n_year = traffic_yr_combinations %>%
#   group_by(years) %>%
#   summarise(n = n())
# # A tibble: 5,165 x 2
# # years                                                      n
# # <chr>                                                  <int>
# #   1 2000,2001                                                 46
# # 2 2000,2001,2002                                            48
# # 3 2000,2001,2002,2003                                       56
# # 4 2000,2001,2002,2003,2004                                 170
# # 5 2000,2001,2002,2003,2004,2005                             60
# # 6 2000,2001,2002,2003,2004,2005,2006                        42
# # 7 2000,2001,2002,2003,2004,2005,2006,2007                   24
# # 8 2000,2001,2002,2003,2004,2005,2006,2007,2008             747
# # 9 2000,2001,2002,2003,2004,2005,2006,2007,2008,2009      12370
# # 10 2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010    11
# # # … with 5,155 more rows
#
# traffic_aadf_yrs = traffic_yr_combinations %>%
#   filter(max_year >= 2016 & min_year <= 2013)
# nrow(traffic_aadf_yrs)
# # [1] 116702
# nrow(traffic_aadf_yrs) / nrow(traffic_cyclable_2) # 70% of the count points
# summary(traffic_aadf_yrs$n_years)
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# # 2.000   6.000  10.000   9.313  11.000  20.000
# # median is 10, so let's filter all those with 10 or more counts for consistency
# traffic_aadf_yrs = traffic_yr_combinations %>%
#   filter(max_year >= 2016 & min_year <= 2013) %>%
#   filter(n_years >= 10)
# nrow(traffic_aadf_yrs)
# # [1] 68117
# nrow(traffic_aadf_yrs) / nrow(traffic_cyclable_2) # 41% of the count points
# summary_n_year = traffic_aadf_yrs %>%
#   group_by(years) %>%
#   summarise(n = n()) %>%
#   ungroup() %>%
#   arrange(desc(n))
# nrow(summary_n_year)
# # [1] 1089
#
# traffic_aadf_yrs %>%
#   count(count_point_id) %>%
#   ggplot() +
#   geom_histogram(aes(n), binwidth = 1)
#
# traffic_aadf_yrs %>%
#   count(year) %>%
#   ggplot() +
#   geom_line(aes(year, n)) +
#   ylim(c(0, NA))
# ggsave("figures/aadf-counts-per-year-10+yrs.png")
#
#
# # data from 2010 onwards --------------------------------------------------
#
# traffic_aadf_yrs = traffic_yr_combinations %>%
#   group_by(count_point_id) %>%
#   filter(max_year >= 2016 & min_year <= 2013) %>%
#   filter(year >= 2010) %>%
#   mutate(n_years = length(unique(year))) %>%
#   filter(n_years >= 6) %>%
#   ungroup()
# nrow(traffic_aadf_yrs)
# # [1] 49223
# length(unique(traffic_aadf_yrs$count_point_id))
# # [1] 5361
# nrow(traffic_aadf_yrs) / nrow(traffic_cyclable_2) # 41% of the count points
#
# traffic_aadf_yrs %>%
#   count(count_point_id) %>%
#   ggplot() +
#   geom_histogram(aes(n), binwidth = 1)
# ggsave("figures/aadf-counts-per-year-2010-2019-6-min.png")
#
# traffic_aadf_yrs %>%
#   count(year) %>%
#   ggplot() +
#   geom_line(aes(year, n)) +
#   ylim(c(0, NA)) +
#   scale_x_continuous(breaks = 2010:2019)
# ggsave("figures/aadf-counts-per-year-10+yrs.png")

# complete 2010:2019 data --------------------------------------------------

traffic_2010_on = traffic_cyclable %>%
  filter(year >= 2010)

traffic_aadf_yrs = traffic_cyclable %>%
  group_by(count_point_id) %>%
  filter(year >= 2010) %>%
  mutate(n_years = length(unique(year))) %>%
  filter(n_years >= 10) %>%
  ungroup()
nrow(traffic_aadf_yrs)
# [1] 30490
length(unique(traffic_aadf_yrs$count_point_id))
# [1] 3049
nrow(traffic_aadf_yrs) / nrow(traffic_2010_on) # 35% of the count points

traffic_aadf_yrs %>%
  count(year) %>%
  ggplot() +
  geom_line(aes(year, n)) +
  ylim(c(0, NA)) +
  scale_x_continuous(breaks = 2010:2019)
ggsave("figures/aadf-counts-per-year-2010:2019-inclusive.png")

table(traffic_aadf$road_category)
table(traffic_aadf$road_type)
traffic_long = traffic_aadf_yrs %>%
  select(year, pedal_cycles, cars_and_taxis, lgvs, all_hgvs, road_category) %>%
  group_by(year, road_category) %>%
  summarise_all(mean) %>%
  pivot_longer(!matches("year|cat"), names_to = "mode", values_to = "count")
traffic_long %>%
  ggplot() +
  geom_line(aes(year, count, col = mode)) +
  facet_wrap(~road_category, scales = "free") +
  scale_y_log10() +
  scale_x_continuous(breaks = c(2010, 2015, 2019))
ggsave("figures/facet-mean-aadf-road-category.png")

traffic_aadf_yrs_la_summary = traffic_aadf_yrs %>%
  group_by(year, local_authority_name) %>%
  summarise_at(
    .vars = vars(pedal_cycles, cars_and_taxis, lgvs, all_hgvs),
    .funs = list(mean = mean, length)
    ) %>%
  rename(n_counters = pedal_cycles_fn1) %>%
  select(-matches("fn1")) %>%
  mutate(cycling_2011 = case_when(
    year == 2011 ~ pedal_cycles_mean
  )) %>%
  ungroup() %>%
  group_by(local_authority_name) %>%
  mutate(multiplier = pedal_cycles_mean / mean(cycling_2011, na.rm = TRUE)) %>%
  select(-cycling_2011)

traffic_aadf_weighted_mean = traffic_aadf_yrs_la_summary %>%
  group_by(year) %>%
  summarise(mean_uptake = weighted.mean(multiplier, w = n_counters, na.rm = TRUE))

ggplot(traffic_aadf_yrs_la_summary) +
  geom_line(aes(year, multiplier, group = local_authority_name, size = n_counters), alpha = 0.1) +
  geom_hline(yintercept = 1, colour = "white") +
  geom_line(aes(year, mean_uptake), data = traffic_aadf_weighted_mean, col = "red") +
  scale_x_continuous(breaks = c(2011, 2015, 2019))

ggsave("figures/aadf-uptake-2011-multiplier-line-mean.png")
readr::write_csv(traffic_aadf_yrs_la_summary, "small-output-datasets/traffic_aadf_yrs_la_summary.csv")
dir.create("small-output-datasets")


# Count roads by category -------------------------------------------------

traffic_aadf_yrs %>%
  group_by(road_category) %>%
  tally()

traffic_2010_on %>%
  group_by(road_category) %>%
  tally()

dim(traffic_2010_on)
ttt = traffic_cyclable %>% filter(road_category == "TA")
dim(ttt)
dim(ttt[ttt$pedal_cycles == 0,])

# Make map of LAs ---------------------------------------------------------

# download uas/counties - bfc
u = "https://opendata.arcgis.com/datasets/43b324dc1da74f418261378a9a73227f_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
# ultra-generalised - not used
# u = "https://opendata.arcgis.com/datasets/b216b4c8a4e74f6fb692a1785255d777_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
dir.create("counties-uas-2019-bfc")
setwd("counties-uas-2019-bfc/")
counties_gb = ukboundaries::duraz(u)
setwd("..")
getwd() # in the right directory again 🎉
counties_uas_gb = counties_gb %>% filter(!str_detect(string = ctyua19cd, "N"))
saveRDS(counties_uas_gb, "counties_uas_gb_2019_bfc.Rds")
mapview::mapview(counties_uas_gb)


traffic_aadf_sf = traffic_cyclable %>%
  group_by(count_point_id, local_authority_name) %>%
  mutate(easting = mean(easting), northing = mean(northing)) %>%
  group_by(count_point_id, local_authority_name, easting, northing) %>%
  summarise_at(vars(pedal_cycles:all_motor_vehicles), .funs = mean) %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700)
nrow(traffic_aadf_sf)
# [1] 42541 ([1] 41980 when local_authority_name is omitted)
summary(sf::st_geometry_type(traffic_aadf_sf))
mapview::mapview(traffic_aadf_sf)

# county aggregation
traffic_aadf_sf_las = traffic_cyclable %>%
  group_by(local_authority_name) %>%
  mutate(easting = mean(easting), northing = mean(northing)) %>%
  group_by(local_authority_name, easting, northing) %>%
  summarise_at(vars(pedal_cycles:all_motor_vehicles), .funs = mean) %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700)
nrow(traffic_aadf_sf_las)
# [1] 210
mapview::mapview(traffic_aadf_sf_las)
traffic_aadf_sf_las_joined = sf::st_join(
  traffic_aadf_sf_las,
  counties_uas_gb %>% select(name = ctyua19nm)
  )
aadf_la_county_lookup = traffic_aadf_sf_las_joined %>%
  select(name, local_authority_name) %>%
  filter(name != local_authority_name) %>%
  sf::st_drop_geometry()
readr::write_csv(aadf_la_county_lookup, "small-output-datasets/aadf_la_county_lookup.csv")

# join and create lookup at counter id level
traffic_aadf_sf_las_joined = sf::st_join(
  traffic_aadf_sf,
  counties_uas_gb %>% select(name = ctyua19nm)
)

aadf_la_county_lookup_point = traffic_aadf_sf_las_joined %>%
  select(count_point_id, name, local_authority_name) %>%
  filter(name != local_authority_name)

aadf_la_county_lookup_point
# identify edge case points in aggregated data:
# aadf_la_county_lookup_point %>%
#   filter(name == "Swansea" & local_authority_name == "Carmarthenshire") %>%
#   mapview::mapview() +
#   mapview::mapview(counties_uas_gb)

aadf_la_county_lookup_point_n = aadf_la_county_lookup_point %>%
  sf::st_drop_geometry() %>%
  group_by(name, local_authority_name) %>%
  summarise(n = n()) %>%
  arrange(n)
table(aadf_la_county_lookup_point_n$n)
# 1   2   3   4   5  14  59  63  78  79 106 112 125 145 155 181 204 240 264 304 344 409
# 95  18   2   4   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1
View(aadf_la_county_lookup_point_n)
aadf_la_county_lookup_point_n_filtered = aadf_la_county_lookup_point_n %>%
  filter(n >= 14)
aadf_la_county_lookup_point_n_filtered

summary(sel <- counties_uas_gb$ctyua19nm %in% traffic_aadf_yrs_la_summary$local_authority_name)
counties_gb$ctyua19nm[!sel]

aadf_la_county_lookup_point2 = aadf_la_county_lookup_point %>%
  sf::st_drop_geometry() %>%
  inner_join(., aadf_la_county_lookup_point_n_filtered) %>%
  select(-n)
readr::write_csv(aadf_la_county_lookup_point2, "small-output-datasets/aadf_la_county_lookup.csv")

# Fix points with location errors
error1 = traffic_cyclable %>%
  filter(count_point_id == 946853)
error2 = traffic_cyclable %>%
  filter(count_point_id == 952939)

error1[error1$easting == 135809,] = error1[error1$easting == 135809,] %>%
  mutate(northing = 24870)
error2 = error2 %>%
  mutate(northing = 221460)

traffic_cyclable = traffic_cyclable %>%
  filter(count_point_id != 952939,
         count_point_id != 946853)
traffic_cyclable = rbind(traffic_cyclable, error1, error2)

traffic_cyclable_clean = traffic_cyclable %>%
  left_join(., aadf_la_county_lookup_point2 %>% select(count_point_id, name))
summary(as.factor(traffic_cyclable_clean$name))
summary(as.factor(traffic_cyclable_clean$local_authority_name))
traffic_cyclable_clean$name[is.na(traffic_cyclable_clean$name)] =
  traffic_cyclable_clean$local_authority_name[is.na(traffic_cyclable_clean$name)]
summary(traffic_cyclable_clean$name == traffic_cyclable_clean$local_authority_name)
# Mode   FALSE    TRUE    NA's
# logical   12445  172340    1188
traffic_cyclable_clean_no_la = traffic_cyclable_clean %>%
  filter(is.na(name))
traffic_cyclable_clean_no_la_joined = traffic_cyclable_clean_no_la %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  select(count_point_id) %>%
  sf::st_join(., counties_uas_gb %>% select(name_updated = ctyua19nm)) %>%
  sf::st_drop_geometry()

table(traffic_cyclable_clean_no_la_joined$name_updated)
# Glasgow City North Lanarkshire South Lanarkshire
# 611               562                 1

traffic_cyclable_clean = traffic_cyclable_clean %>%
  left_join(., traffic_cyclable_clean_no_la_joined)
summary(as.factor(traffic_cyclable_clean$name_updated))
summary(as.factor(traffic_cyclable_clean$name))

traffic_cyclable_clean$name[is.na(traffic_cyclable_clean$name)] =
  traffic_cyclable_clean$name_updated[is.na(traffic_cyclable_clean$name)]
summary(as.factor(traffic_cyclable_clean$name))
traffic_cyclable_clean %>%
  filter(is.na(name)) %>%
  select(count_point_id, name, easting) %>%
  count(count_point_id)

# where is it?
missing_count_point = traffic_aadf_sf %>%
  filter(count_point_id == 50974)

mapview::mapview(missing_count_point)

traffic_cyclable_clean$name[traffic_cyclable_clean$count_point_id == 50974] =
  "Glasgow City"

saveRDS(traffic_cyclable_clean, "traffic_cyclable_clean.Rds")
piggyback::pb_upload("traffic_cyclable_clean.Rds")

# test code ---------------------------------------------------------------

# # check nrow of different county/ua datasets
# # rapid regions?
# u = "https://opendata.arcgis.com/datasets/54a0620552824e32af97d476b83ca18d_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
# dir.create("counties-uas-2011")
# setwd("counties-uas-2011")
# counties_gb = ukboundaries::duraz(u)
# nrow(counties_gb)
# # [1] 174
# setwd("..")
# getwd() # in the right directory again 🎉
# summary(sel <- counties_gb$ctyua19nm %in% traffic_aadf_yrs_la_summary$local_authority_name)
# summary(traffic_aadf_yrs_la_summary$local_authority_name %in% counties_gb$ctyua19nm)
# lads_not_in_aadf1 = counties_gb[!sel, ]
# mapview::mapview(lads_not_in_aadf1)


# dir.create("counties-2019")
# setwd("counties-2019")
# counties_gb = ukboundaries::duraz("https://opendata.arcgis.com/datasets/7a7c4e834b4c493d9c011b4f3d144698_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D")
# mapview::mapview(counties_gb) # missing loads!
# setwd("..")
#
#
#
# traffic_cyclable_years = traffic_cyclable_2 %>%
#   group_by(latitude, longitude) %>%
#   mutate(
#     present_2009 = year %in% 2009,
#     present_2010 = year %in% 2010,
#     present_2011 = year %in% 2011,
#     present_2012 = year %in% 2012,
#     present_2013 = year %in% 2013,
#     present_2014 = year %in% 2014,
#     present_2015 = year %in% 2015,
#     present_2016 = year %in% 2016,
#     present_2017 = year %in% 2017,
#     present_2018 = year %in% 2018,
#     present_2019 = year %in% 2019
#     )
# nrow(traffic_cyclable_years)
# # [1] 166680
# traffic_cyclable_years %>%
#   filter(year == 2009) %>%
#   select(year, present_2009:present_2019)
#
# summary(traffic_cyclable_years %>% select(present_2009:present_2019))
#
# summary_present_years = traffic_cyclable_years %>%
#   group_by(year) %>%
#   summarise(
#     n = n(),
#     present_2009 = sum(present_2009),
#     present_2010 = sum(present_2010),
#     present_2011 = sum(present_2011),
#     present_2012 = sum(present_2012)
#     )
# ggplot(summary_present_years) +
#   geom_line(aes(year, n, col = present_2009))

# lads = readRDS("lads.Rds")
# nrow(lads)
# length(unique(traffic_aadf$local_authority_name))
# summary(sel <- lads$Name %in% traffic_aadf_yrs_la_summary$local_authority_name)
# # Mode   FALSE    TRUE
# # logical     219     163
# summary(traffic_aadf_yrs_la_summary$local_authority_name %in% lads$Name)
# # Mode   FALSE    TRUE
# # logical     380    1625
# lads_in_aadf1 = lads[sel, ]
# mapview::mapview(lads_in_aadf1)

# # try 2018 definition of uas/counties
# u = "https://opendata.arcgis.com/datasets/d13feea979be44eb83bceeed94a1510d_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D"
# dir.create("counties-uas-2018")
# setwd("counties-uas-2018")
# counties_gb = ukboundaries::duraz(u)
# setwd("..")
# saveRDS(counties_gb, "counties-uas-2018.Rds")
# getwd() # in the right directory again 🎉
# counties_gb = counties_gb %>% filter(!str_detect(string = cauth18cd, "N"))
# nrow(counties_gb)
# mapview::mapview(counties_gb) # missing loads!
# nrow(counties_gb)
