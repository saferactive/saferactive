# Aim: generate descriptive statistics and visualisations of the AADF data, building on dft-aadf.R

library(tidyverse)
traffic_cyclable = readRDS("traffic_cyclable_clean.Rds")
dim(traffic_cyclable) #183884


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

