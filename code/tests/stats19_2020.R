library(tidyverse)
devtools::install_github("ropensci/stats19")
library(stats19)

# # Full data on the months up until June 2020
# # No good because I can't identify the casualty mode (cyclist/pedestrian/car occupant etc)
# crashes_2020_halfyear = get_stats19(year = 2020, type = "ac")

# Read in LA stats19 2020 KSI
# This is summary data and I can't specify peak hour crashes only. Therefore I will just compare it with the full data for 2019
stats19_2020 = read_csv("stats19_2020.csv")
stats19_2019 = read_csv("stats19_2019.csv")

stats19_2019 = stats19_2019 %>%
  select(`ONS Code`, `Region/Local Authority`, Cyclist) %>%
  rename(bicycle_2019 = Cyclist, ONS_Code = `ONS Code`, Region = `Region/Local Authority`)
stats19_2020 = stats19_2020 %>%
  select(`ONS Code`, `Region/Local Authority`, Cyclist) %>%
  rename(bicycle_2020 = Cyclist, ONS_Code = `ONS Code`, Region = `Region/Local Authority`)

stats19_compare = inner_join(stats19_2019, stats19_2020, by = c("ONS_Code", "Region"))

