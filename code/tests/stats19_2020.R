library(tidyverse)
devtools::install_github("ropensci/stats19")
library(stats19)

# # Full data on the months up until June 2020
# # No good because I can't identify the casualty mode (cyclist/pedestrian/car occupant etc)
# crashes_2020_halfyear = get_stats19(year = 2020, type = "ac")

# Read in LA stats19 2020 KSI
# This is summary data and I can't specify peak hour crashes only. Therefore I will just compare it with the full data for 2019
# The data is for adjusted KSI
stats19_2020 = read_csv("stats19_2020.csv")
stats19_2019 = read_csv("stats19_2019.csv")

stats19_2019 = stats19_2019 %>%
  select(`ONS Code`, `Region/Local Authority`, Cyclist) %>%
  rename(bicycle_2019 = Cyclist, ONS_Code = `ONS Code`, Region = `Region/Local Authority`)
stats19_2020 = stats19_2020 %>%
  select(`ONS Code`, `Region/Local Authority`, Cyclist) %>%
  rename(bicycle_2020 = Cyclist, ONS_Code = `ONS Code`, Region = `Region/Local Authority`)

stats19_compare = inner_join(stats19_2019, stats19_2020, by = c("ONS_Code", "Region"))

# correct LA names and codes
stats19_compare$Region = gsub( "&", "and", stats19_compare$Region)
stats19_compare$ONS_Code[which(stats19_compare$Region == "Gateshead")] = "E08000037"
stats19_compare$ONS_Code[which(stats19_compare$Region == "Northumberland")] = "E06000057"
stats19_compare$ONS_Code[which(stats19_compare$Region == "Buckinghamshire")] = "E10000002"
stats19_compare$ONS_Code[which(stats19_compare$Region == "London Airport")] = "E09000017"
stats19_compare$ONS_Code[which(stats19_compare$Region == "East Dunbartonshire")] = "S12000045"
stats19_compare$ONS_Code[which(stats19_compare$Region == "Fife")] = "S12000047"
stats19_compare$ONS_Code[which(stats19_compare$Region == "Perth and Kinross")] = "S12000048"
stats19_compare$Region[which(stats19_compare$Region == "London Airport")] = "Hillingdon"

# Group Hillingdon and London Airport together
stats19_compare = stats19_compare %>%
  group_by(ONS_Code, Region) %>%
  summarise(bicycle_2019 = sum(bicycle_2019),
            bicycle_2020 = sum(bicycle_2020))

# Calculate % change
stats19_compare = stats19_compare %>%
  mutate(pchange = bicycle_2020/bicycle_2019)

# Prepare to join with spatial data
bounds = read_sf("Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC.shp")
# a = bounds$ctyua19cd
# b = stats19_compare$ONS_Code
# aa = a[! a %in% b]
# bb = b[! b %in% a]
# bounds %>% filter(ctyua19cd %in% aa)
# stats19_compare %>% filter(ONS_Code %in% bb)

stats19_compare = inner_join(bounds, stats19_compare, by = c("ctyua19cd" = "ONS_Code"))

saveRDS(stats19_compare, "stats19_compare.Rds")
