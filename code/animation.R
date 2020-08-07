library(tmap)
library(dplyr)
library(sf)
tmap_mode("view")


# Count length of journeys within each London Borough
piggyback::pb_download_url("lads.Rds")
# [1] "https://github.com/saferactive/saferactive/releases/download/0.1/lads.Rds"
piggyback::pb_download("lads.Rds")
lads = readRDS("lads.Rds")
boroughs = as.character(spData::lnd$NAME)

lads = lads %>%
  filter(Name %in% boroughs)

crashes_active_london = readRDS("crashes_active_london.Rds")

crashes_active_london %>%
  group_by()

summary(crashes_active_london$year)



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
crashes_m2015 = filter(crashes_active_london, year %in% years)

midyear = 2016
years = ((midyear-2):(midyear+2))
crashes_m2016 = filter(crashes_active_london, year %in% years)


##
# years = 2011
# crashes_m2011 = filter(crashes_active_london, year %in% years)
#
# years = 2012
# crashes_m2012 = filter(crashes_active_london, year %in% years)
#
# years = 2013
# crashes_m2013 = filter(crashes_active_london, year %in% years)
#
# years = 2014
# crashes_m2014 = filter(crashes_active_london, year %in% years)
#
# years = 2015
# crashes_m2015 = filter(crashes_active_london, year %in% years)
#
# years = 2016
# crashes_m2016 = filter(crashes_active_london, year %in% years)
#
# years = 2017
# crashes_m2017 = filter(crashes_active_london, year %in% years)
#
# years = 2018
# crashes_m2018 = filter(crashes_active_london, year %in% years)

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

# crashes_per_borough_2017 = crashes_m2017 %>%
#   group_by(local_authority_district) %>%
#   summarise(year = 2017,
#             cycle_fatal = sum(cycle_fatal),
#             cycle_adjusted_serious = sum(cycle_adjusted_serious),
#             cycle_adjusted_slight = sum(cycle_adjusted_slight),
#             cycle_KSI = cycle_fatal + cycle_adjusted_serious,
#             ped_fatal = sum(ped_fatal),
#             ped_adjusted_serious = sum(ped_adjusted_serious),
#             ped_adjusted_slight = sum(ped_adjusted_slight),
#             ped_KSI = ped_fatal + ped_adjusted_serious) %>%
#   st_drop_geometry()
#
# crashes_per_borough_2018 = crashes_m2018 %>%
#   group_by(local_authority_district) %>%
#   summarise(year = 2018,
#             cycle_fatal = sum(cycle_fatal),
#             cycle_adjusted_serious = sum(cycle_adjusted_serious),
#             cycle_adjusted_slight = sum(cycle_adjusted_slight),
#             cycle_KSI = cycle_fatal + cycle_adjusted_serious,
#             ped_fatal = sum(ped_fatal),
#             ped_adjusted_serious = sum(ped_adjusted_serious),
#             ped_adjusted_slight = sum(ped_adjusted_slight),
#             ped_KSI = ped_fatal + ped_adjusted_serious) %>%
#   st_drop_geometry()



crashes_per_borough = rbind(crashes_per_borough_2011, crashes_per_borough_2012, crashes_per_borough_2013, crashes_per_borough_2014,  crashes_per_borough_2015,  crashes_per_borough_2016
                            # , crashes_per_borough_2017, crashes_per_borough_2018
                            )

crashes_per_borough = inner_join(lads, crashes_per_borough, by = c("Name" = "local_authority_district")) %>%
  dplyr::select(-Level)

tm_shape(crashes_per_borough) +
  tm_polygons("cycle_KSI", title = "Total cyclists KSI") +
  tm_facets(by = "year")

anim = tm_shape(crashes_per_borough) +
  tm_polygons("cycle_KSI", title = "Total cyclists KSI") +
  tm_facets(along = "year", free.coords = FALSE)
tmap_animation(anim, filename = "anim.gif", delay = 200)

browseURL("anim.gif")
piggyback::pb_upload("anim.gif")
piggyback::pb_download_url("anim.gif")
# [1] "https://github.com/saferactive/saferactive/releases/download/0.1/anim.gif"

urb_anim = tm_shape(world) + tm_polygons() +
  tm_shape(urban_agglomerations) + tm_dots(size = "population_millions") +
  tm_facets(by = "year")
