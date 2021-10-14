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
stats19_2018 = read_csv("stats19_2018.csv")
stats19_2017 = read_csv("stats19_2017.csv")
stats19_2016 = read_csv("stats19_2016.csv")
stats19_2015 = read_csv("stats19_2015.csv")
stats19_2014 = read_csv("stats19_2014.csv")
stats19_2013 = read_csv("stats19_2013.csv")

stats19_2019 = stats19_2019 %>%
  select(`ONS Code`, `Region/Local Authority`, Cyclist, Pedestrian) %>%
  rename(bicycle_2019 = Cyclist, foot_2019 = Pedestrian, ONS_Code = `ONS Code`, Region = `Region/Local Authority`)
stats19_2020 = stats19_2020 %>%
  select(`ONS Code`, `Region/Local Authority`, Cyclist, Pedestrian) %>%
  rename(bicycle_2020 = Cyclist, foot_2020 = Pedestrian, ONS_Code = `ONS Code`, Region = `Region/Local Authority`)
stats19_2018 = stats19_2018 %>%
  select(`ONS Code`, `Region/Local Authority`, Cyclist, Pedestrian) %>%
  rename(bicycle_2018 = Cyclist, foot_2018 = Pedestrian, ONS_Code = `ONS Code`, Region = `Region/Local Authority`)
stats19_2017 = stats19_2017 %>%
  select(`ONS Code`, `Region/Local Authority`, Cyclist, Pedestrian) %>%
  rename(bicycle_2017 = Cyclist, foot_2017 = Pedestrian, ONS_Code = `ONS Code`, Region = `Region/Local Authority`)
stats19_2016 = stats19_2016 %>%
  select(`ONS Code`, `Region/Local Authority`, Cyclist, Pedestrian) %>%
  rename(bicycle_2016 = Cyclist, foot_2016 = Pedestrian, ONS_Code = `ONS Code`, Region = `Region/Local Authority`)
stats19_2015 = stats19_2015 %>%
  select(`ONS Code`, `Region/Local Authority`, Cyclist, Pedestrian) %>%
  rename(bicycle_2015 = Cyclist, foot_2015 = Pedestrian, ONS_Code = `ONS Code`, Region = `Region/Local Authority`)
stats19_2014 = stats19_2014 %>%
  select(`ONS Code`, `Region/Local Authority`, Cyclist, Pedestrian) %>%
  rename(bicycle_2014 = Cyclist, foot_2014 = Pedestrian, ONS_Code = `ONS Code`, Region = `Region/Local Authority`)
stats19_2013 = stats19_2013 %>%
  select(`ONS Code`, `Region/Local Authority`, Cyclist, Pedestrian) %>%
  rename(bicycle_2013 = Cyclist, foot_2013 = Pedestrian, ONS_Code = `ONS Code`, Region = `Region/Local Authority`)

stats19_compare = inner_join(stats19_2020, stats19_2019, by = c("ONS_Code", "Region")) %>%
  inner_join(stats19_2018, by = c("ONS_Code", "Region")) %>%
  inner_join(stats19_2017, by = c("ONS_Code", "Region")) %>%
  inner_join(stats19_2016, by = c("ONS_Code", "Region")) %>%
  inner_join(stats19_2015, by = c("ONS_Code", "Region")) %>%
  inner_join(stats19_2014, by = c("ONS_Code", "Region")) %>%
  inner_join(stats19_2013, by = c("ONS_Code", "Region"))

####
stats19_compare_regions = stats19_compare %>%
  rename(region = Region)

stats19_compare_regions$region = gsub("Eastern", "East of England", stats19_compare_regions$region)
stats19_compare_regions$region = gsub("Yorkshire/Humberside", "Yorkshire and The Humber", stats19_compare_regions$region)

stats19_compare_cycle = pivot_longer(stats19_compare_regions, cols = c(bicycle_2020, bicycle_2019, bicycle_2018, bicycle_2017, bicycle_2016, bicycle_2015, bicycle_2014, bicycle_2013), names_to = "year", names_prefix = "bicycle_", values_to = "ksi_cycle")
stats19_compare_foot = pivot_longer(stats19_compare_regions, cols = c(foot_2020, foot_2019, foot_2018, foot_2017, foot_2016, foot_2015, foot_2014, foot_2013), names_to = "year", names_prefix = "foot_", values_to = "ksi_foot")
stats19_compare_cycle$year = as.numeric(stats19_compare_cycle$year)
stats19_compare_foot$year = as.numeric(stats19_compare_foot$year)
stats19_compare_cycle = stats19_compare_cycle %>%
  select(ONS_Code, region, year, ksi_cycle)
stats19_compare_foot = stats19_compare_foot %>%
  select(ONS_Code, region, year, ksi_foot)
stats19_compare_regions = inner_join(stats19_compare_cycle, stats19_compare_foot, by = c("ONS_Code", "region", "year"))

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
  mutate(pchange = case_when(
    bicycle_2020 == 0 ~ 1,
    bicycle_2019 == 0 ~ 1,
    TRUE ~ bicycle_2020/bicycle_2019
    ))

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


