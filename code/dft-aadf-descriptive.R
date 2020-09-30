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
