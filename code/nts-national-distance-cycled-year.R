# Aim: estimate cycling levels across England and Wales
library(tidyverse)

u = "https://www.ons.gov.uk/generator?format=csv&uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/timeseries/enpop/pop"
population_england = readr::read_csv(u, skip = 7)
population_england
names(population_england) = c("year", "population")
readr::write_csv(population_england, "small-output-datasets/population_england.csv")

# percent of population living in wales given that its population is ~3.1 million
u = "https://www.ons.gov.uk/generator?format=csv&uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/timeseries/wapop/pop"
population_wales = readr::read_csv(u, skip = 7)
population_england$population_wales = population_wales[[2]]
population_england$wales_multiplier = (population_england$population + population_england$population_wales) / population_england$population
plot(population_england$wales_multiplier)

# percent of population living in scotland given that its population is ~3.1 million
u = "https://www.ons.gov.uk/generator?format=csv&uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/timeseries/scpop/pop"
population_scotland = readr::read_csv(u, skip = 7)
population_england$population_scotland = population_scotland[[2]]
population_england$scotland_multiplier = (population_england$population + population_england$population_scotland) / population_england$population
plot(population_england$scotland_multiplier)

u = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/905948/nts0303.ods"
f = basename(u)
download.file(u, f)
nts_data = readODS::read_ods(f, skip = 100)
nts_dist = nts_data[1:17, ]
nts_dist
sapply(nts_dist, class)
nts_dist[[2]] = as.numeric(nts_dist[[2]])
nts_dist_long = tidyr::pivot_longer(nts_dist, !Mode, names_to = "year", values_to = "miles_person_yr")

population_england$year = as.character(population_england$year)
distance_cycled = nts_dist_long %>%
  filter(Mode == "Bicycle")
distance_cycled_joined = inner_join(distance_cycled, population_england)
distance_cycled_total = distance_cycled_joined %>%
  mutate(km_cycled_yr_ew = miles_person_yr * population * wales_multiplier * 1.61,
         year = as.integer(year))
distance_cycled_total # in 2011: 4475741485
summary(distance_cycled_total$km_cycled_yr_ew)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 3.273e+09 3.685e+09 4.361e+09 4.362e+09 5.016e+09 5.628e+09
distance_cycled_total = distance_cycled_total %>%
  mutate(km_cycled_yr_gb = miles_person_yr * (population + population_wales + population_scotland) * 1.61)
plot(distance_cycled_total$year, distance_cycled_total$km_cycled_yr_ew, ylim = c(0, 6e9))
points(distance_cycled_total$year, distance_cycled_total$km_cycled_yr_gb, ylim = c(0, 6e9))
summary(distance_cycled_total$km_cycled_yr_gb)
readr::write_csv(distance_cycled_total, "small-output-datasets/distance_cycled_total.csv")

# compare with dft cycle counter data
u = "http://data.dft.gov.uk.s3.amazonaws.com/road-traffic/region_traffic.csv"
cycle_counter_aggregated = readr::read_csv(u)
cycle_counters_gb = cycle_counter_aggregated %>%
  group_by(year) %>%
  summarise(across(c(pedal_cycles, all_motor_vehicles), sum))
g1 = ggplot(distance_cycled_total %>% ungroup()) +
  geom_point(aes(year, km_cycled_yr_gb)) +
  geom_smooth(aes(x = year, y = km_cycled_yr_gb))
g2 = ggplot(cycle_counters_gb) +
  geom_point(aes(year, pedal_cycles)) +
  geom_smooth(aes(year, pedal_cycles)) +
  xlim(range(distance_cycled_total$year))
library(patchwork)

g1 + g2
# ggsave(filename = "figures/nts-counter-comparison.png")
