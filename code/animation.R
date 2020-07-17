library(tmap)
library(dplyr)
library(sf)
tmap_mode("view")


# Count length of journeys within each London Borough
lads = readRDS("lads.Rds")

boroughs = c(
  "Camden",
  "Greenwich",
  "Hackney",
  "Hammersmith and Fulham",
  "Islington",
  "Kensington and Chelsea",
  "Lambeth",
  "Lewisham",
  "Southwark",
  "Tower Hamlets",
  "Wandsworth",
  "Westminster",
  "Barking and Dagenham",
  "Barnet",
  "Bexley",
  "Brent",
  "Bromley",
  "Croydon",
  "Ealing",
  "Enfield",
  "Haringey",
  "Harrow",
  "Havering",
  "Hillingdon",
  "Hounslow",
  "Kingston upon Thames",
  "Merton",
  "Newham",
  "Redbridge",
  "Richmond upon Thames",
  "Sutton",
  "Waltham Forest",
  "City of London")

lads = lads %>%
  filter(Name %in% boroughs)

crashes_active_london = readRDS("crashes_active_london.Rds")

# allyears = unique(crashes_active_london$year)

# for (i in (min(allyears)+2):(max(allyears)-2)){
# years = ((i-2):(i+2))
# crashes_i = filter(crashes_active_london, year %in% years)
# crashes_per_borough_i = crashes_i %>%
#   group_by(local_authority_district) %>%
#   summarise(nam = paste("cycle_fatal",i, sep = ""),
#             assign(nam, sum(cycle_fatal),
#             cycle_adjusted_serious = sum(cycle_adjusted_serious),
#             cycle_adjusted_slight = sum(cycle_adjusted_slight),
#             cycle_KSI = cycle_fatal + cycle_adjusted_serious,
#             ped_fatal = sum(ped_fatal),
#             ped_adjusted_serious = sum(ped_adjusted_serious),
#             ped_adjusted_slight = sum(ped_adjusted_slight),
#             ped_KSI = ped_fatal + ped_adjusted_serious) %>%
#   st_drop_geometry()
# crashes_per_borough = inner_join(crashes_per_borough, crashes_per_borough_i, )
# }

midyear = 2011
years = ((midyear-2):(midyear+2))
crashes_m2011 = filter(crashes_active_london, year %in% years)

midyear = 2012
years = ((midyear-2):(midyear+2))
crashes_m2012 = filter(crashes_active_london, year %in% years)

midyear = 2013
years = ((midyear-2):(midyear+2))
crashes_m2013 = filter(crashes_active_london, year %in% years)

midyear = 2014
years = ((midyear-2):(midyear+2))
crashes_m2014 = filter(crashes_active_london, year %in% years)

midyear = 2015
years = ((midyear-2):(midyear+2))
crashes_m = filter(crashes_active_london, year %in% years)

midyear = 2016
years = ((midyear-2):(midyear+2))
crashes_m2016 = filter(crashes_active_london, year %in% years)

crashes_per_borough_2011 = crashes_m2011 %>%
  group_by(local_authority_district) %>%
  summarise(year = 2011,
            cycle_fatal = sum(cycle_fatal),
            cycle_adjusted_serious = sum(cycle_adjusted_serious),
            cycle_adjusted_slight = sum(cycle_adjusted_slight),
            cycle_KSI = cycle_fatal + cycle_adjusted_serious,
            ped_fatal = sum(ped_fatal),
            ped_adjusted_serious = sum(ped_adjusted_serious),
            ped_adjusted_slight = sum(ped_adjusted_slight),
            ped_KSI = ped_fatal + ped_adjusted_serious) %>%
  st_drop_geometry()

crashes_per_borough_2012 = crashes_m2012 %>%
  group_by(local_authority_district) %>%
  summarise(year = 2012,
            cycle_fatal = sum(cycle_fatal),
            cycle_adjusted_serious = sum(cycle_adjusted_serious),
            cycle_adjusted_slight = sum(cycle_adjusted_slight),
            cycle_KSI = cycle_fatal + cycle_adjusted_serious,
            ped_fatal = sum(ped_fatal),
            ped_adjusted_serious = sum(ped_adjusted_serious),
            ped_adjusted_slight = sum(ped_adjusted_slight),
            ped_KSI = ped_fatal + ped_adjusted_serious) %>%
  st_drop_geometry()

crashes_per_borough_2013 = crashes_m2013 %>%
  group_by(local_authority_district) %>%
  summarise(year = 2013,
            cycle_fatal = sum(cycle_fatal),
            cycle_adjusted_serious = sum(cycle_adjusted_serious),
            cycle_adjusted_slight = sum(cycle_adjusted_slight),
            cycle_KSI = cycle_fatal + cycle_adjusted_serious,
            ped_fatal = sum(ped_fatal),
            ped_adjusted_serious = sum(ped_adjusted_serious),
            ped_adjusted_slight = sum(ped_adjusted_slight),
            ped_KSI = ped_fatal + ped_adjusted_serious) %>%
  st_drop_geometry()

crashes_per_borough_2014 = crashes_m2014 %>%
  group_by(local_authority_district) %>%
  summarise(year = 2014,
            cycle_fatal = sum(cycle_fatal),
            cycle_adjusted_serious = sum(cycle_adjusted_serious),
            cycle_adjusted_slight = sum(cycle_adjusted_slight),
            cycle_KSI = cycle_fatal + cycle_adjusted_serious,
            ped_fatal = sum(ped_fatal),
            ped_adjusted_serious = sum(ped_adjusted_serious),
            ped_adjusted_slight = sum(ped_adjusted_slight),
            ped_KSI = ped_fatal + ped_adjusted_serious) %>%
  st_drop_geometry

crashes_per_borough_2015 = crashes_m2015 %>%
  group_by(local_authority_district) %>%
  summarise(year = 2015,
            cycle_fatal = sum(cycle_fatal),
            cycle_adjusted_serious = sum(cycle_adjusted_serious),
            cycle_adjusted_slight = sum(cycle_adjusted_slight),
            cycle_KSI = cycle_fatal + cycle_adjusted_serious,
            ped_fatal = sum(ped_fatal),
            ped_adjusted_serious = sum(ped_adjusted_serious),
            ped_adjusted_slight = sum(ped_adjusted_slight),
            ped_KSI = ped_fatal + ped_adjusted_serious) %>%
  st_drop_geometry

crashes_per_borough_2016 = crashes_m2016 %>%
  group_by(local_authority_district) %>%
  summarise(year = 2016,
            cycle_fatal = sum(cycle_fatal),
            cycle_adjusted_serious = sum(cycle_adjusted_serious),
            cycle_adjusted_slight = sum(cycle_adjusted_slight),
            cycle_KSI = cycle_fatal + cycle_adjusted_serious,
            ped_fatal = sum(ped_fatal),
            ped_adjusted_serious = sum(ped_adjusted_serious),
            ped_adjusted_slight = sum(ped_adjusted_slight),
            ped_KSI = ped_fatal + ped_adjusted_serious) %>%
  st_drop_geometry()

crashes_per_borough = rbind(crashes_per_borough_2011, crashes_per_borough_2012, crashes_per_borough_2013, crashes_per_borough_2014,  crashes_per_borough_2015,  crashes_per_borough_2016)

crashes_per_borough = inner_join(lads, crashes_per_borough, by = c("Name" = "local_authority_district")) %>%
  dplyr::select(-Level)

anim = tm_shape(crashes_per_borough) +
  tm_polygons("cycle_KSI", title = "Total cyclists KSI") +
  tm_facets(along = "year", free.coords = FALSE)
tmap_animation(anim, filename = "anim.gif", delay = 25)


urb_anim = tm_shape(world) + tm_polygons() +
  tm_shape(urban_agglomerations) + tm_dots(size = "population_millions") +
  tm_facets(by = "year")
