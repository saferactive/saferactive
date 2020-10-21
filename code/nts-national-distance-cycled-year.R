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
  mutate(km_cycled_yr = miles_person_yr * population * wales_multiplier * 0.62)
summary(distance_cycled_total$km_cycled_yr)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
# 1.260e+09 1.419e+09 1.679e+09 1.680e+09 1.932e+09 2.167e+09
plot(distance_cycled_total$year, distance_cycled_total$km_cycled_yr, ylim = c(0, 2.2e9))
readr::write_csv(distance_cycled_total, "small-output-datasets/distance_cycled_total.csv")
