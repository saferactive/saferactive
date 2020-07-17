# Get pct commute data for London. Ensure journeys to/from outside London are included

library(pct)
library(tidyverse)
library(mapview)
library(sf)
library(units)
library(hms)
library(lubridate)


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
# mapview(lads)



# Cycling to work ---------------------------------------------------------


r = pct::get_pct_rnet(region = "london")

r = r %>%
  filter(bicycle > 0)
# mapview(r)


r$lengths = r %>% st_transform(27700) %>%
  st_length() %>%
  drop_units()

cent = r %>% st_transform(27700) %>%
  st_centroid() %>%
  st_transform(4326)

c_london = st_join(lads, cent, by = st_within)
# mapview(c_london)

c_london = c_london %>%
  mutate(km_cycled = bicycle*lengths/1000)

bike_sum = c_london %>%
  select(Name, km_cycled) %>%
  group_by(Name) %>%
  summarise(km_cycled = sum(km_cycled)) %>%
  st_drop_geometry()


# Walking to work ---------------------------------------------------------

# walk = pct::get_pct_routes_fast(region = "london", purpose = "commute", geography = "lsoa")
#
# walk_lim = walk %>%
#   select(1:12) %>%
#   filter(foot > 0)
#
# write_rds(walk_lim, "walk.Rds")
#
# walk_net = stplanr::overline2(x = walk_lim, "foot")
#
# write_rds(walk_net, "walk_net.Rds")

walk_net = readRDS("walk_net.Rds")

walk_net$lengths = walk_net %>% st_transform(27700) %>%
  st_length() %>%
  drop_units()

walk_cent = walk_net %>% st_transform(27700) %>%
  st_centroid() %>%
  st_transform(4326)

w_london = st_join(lads, walk_cent, by = st_within)
# mapview(c_london)

w_london = w_london %>%
  mutate(km_walked = foot*lengths/1000)

walk_sum = w_london %>%
  select(Name, km_walked) %>%
  group_by(Name) %>%
  summarise(km_walked = sum(km_walked)) %>%
  st_drop_geometry()

##
summed = inner_join(bike_sum, walk_sum, by = "Name")



# Count cycle casualties within each London Borough
crashes_active_london = readRDS("crashes_active_london.Rds")

years = c("2009", "2010", "2011", "2012", "2013")
crashes_2009_13 = filter(crashes_active_london, year %in% years)

# years = c("2014", "2015", "2016", "2017", "2018")
# crashes_2014_18 = filter(crashes_active_london, year %in% years)

# crashes_morning_peak = crashes_2009_13 %>%
#   mutate(datetime = ymd_hms(datetime),
#          time_est = as_hms(datetime)) %>%
#   filter(time_est >= hms::as_hms('07:00:00'),
#          time_est < hms::as_hms('10:00:00'))

crashes_peak = crashes_2009_13 %>%
  mutate(datetime = ymd_hms(datetime),
         time_est = as_hms(datetime)) %>%
  filter((time_est >= hms::as_hms('07:30:00') &
         time_est < hms::as_hms('09:30:00')) |
           (time_est >= hms::as_hms('16:30:00') &
              time_est < hms::as_hms('18:30:00')),
         day_of_week != "Saturday",
         day_of_week != "Sunday")

# changed to use adjusted casualty severity, not accident severity
crashes_per_borough = crashes_peak %>%
  group_by(local_authority_district) %>%
  summarise(cycle_fatal = sum(cycle_fatal),
            cycle_adjusted_serious = sum(cycle_adjusted_serious),
            cycle_adjusted_slight = sum(cycle_adjusted_slight),
            ped_fatal = sum(ped_fatal),
            ped_adjusted_serious = sum(ped_adjusted_serious),
            ped_adjusted_slight = sum(ped_adjusted_slight)) %>%
  st_drop_geometry()

# Calculate casualties per billion km
# the km cycled is from a single day but the crashes are from five years
# would it be better to pick out weekdays only?
# what about seasonal changes in crash rates?
# For smaller geographic areas, we will have to use all crashes not KSI

rate_per_borough = inner_join(crashes_per_borough, summed, by = c("local_authority_district" = "Name")) %>%
  mutate(fatal_per_bkm = (cycle_fatal/261/5/2)/(km_cycled/1000000000),
         serious_per_bkm = (cycle_adjusted_serious/261/5/2)/(km_cycled/1000000000),
         slight_per_bkm = (cycle_adjusted_slight/261/5/2)/(km_cycled/1000000000),
         KSI_per_bkm = ((cycle_fatal/261/5/2)+(cycle_adjusted_serious/365/5/2))/(km_cycled/1000000000),
         total_KSI = cycle_fatal + cycle_adjusted_serious,
         w_fatal_per_bkm = (ped_fatal/261/5/2)/(km_walked/1000000000),
         w_serious_per_bkm = (ped_adjusted_serious/261/5/2)/(km_walked/1000000000),
         w_slight_per_bkm = (ped_adjusted_slight/261/5/2)/(km_walked/1000000000),
         w_KSI_per_bkm = ((ped_fatal/261/5/2)+(ped_adjusted_serious/365/5/2))/(km_walked/1000000000),
         w_total_KSI = ped_fatal + ped_adjusted_serious)

rate_per_borough = inner_join(lads, rate_per_borough, by = c("Name" = "local_authority_district")) %>%
  select(-Level)

rate_per_borough

View(rate_per_borough)

plot(rate_per_borough)

write_rds(rate_per_borough, "rate_per_borough.Rds")

library(tmap)
tmap_mode("view")

##cycle
map1 = tm_shape(rate_per_borough) +
  tm_polygons("KSI_per_bkm", title = "KSI/bkm")
map2 = tm_shape(rate_per_borough) +
  tm_polygons("slight_per_bkm", breaks = c(0, 4000, 8000, 12000, 16000, 20000, 24000), title = "Slight injuries/bkm")
map3 = tm_shape(rate_per_borough) +
  tm_polygons("km_cycled", title = "Km cycled", breaks = c(0, 5000, 10000, 20000, 40000, 60000))
map4 = tm_shape(rate_per_borough) +
  tm_polygons("total_KSI", title = "Total KSI")
tmap_arrange(map1, map2)
tmap_arrange(map3, map4)

##walk
map1 = tm_shape(rate_per_borough) +
  tm_polygons("w_KSI_per_bkm", title = "KSI/bkm")
map2 = tm_shape(rate_per_borough) +
  tm_polygons("w_slight_per_bkm", title = "Slight injuries/bkm")
map3 = tm_shape(rate_per_borough) +
  tm_polygons("km_walked", title = "Km walked", breaks = c(0, 5000, 10000, 20000, 30000, 40000))
map4 = tm_shape(rate_per_borough) +
  tm_polygons("w_total_KSI", title = "Total KSI")
tmap_arrange(map1, map2)
tmap_arrange(map3, map4)

mapview(rate_per_borough["KSI_per_bkm"])

# Exponential decay graph

# library(drc)
# library(aomisc)
#
# negex = drm(KSI_per_bkm ~ km_cycled, data = rate_per_borough, fct = DRC.expoDecay())
#
# plot(negex)

plot(KSI_per_bkm ~ km_cycled, data = rate_per_borough, xlab = "km cycled", ylab = "KSI per bkm 2009 - 2013", ylim = c(500,4100)) # strong exponential decay curve
text(KSI_per_bkm ~ km_cycled, data = rate_per_borough, labels = rate_per_borough$Name, cex = 0.6, font = 2, pos = 3)


plot(w_KSI_per_bkm ~ km_walked, data = rate_per_borough, xlab = "km walked", ylab = "KSI per bkm 2009 - 2013", ylim = c(500,4100)) # strong exponential decay curve
text(w_KSI_per_bkm ~ km_walked, data = rate_per_borough, labels = rate_per_borough$Name, cex = 0.6, font = 2, pos = 3)

# ggplot(df, aes(resp, trt)) +
#   geom_point() +
#   geom_text(aes(label = paste0("(", resp, ")")), nudge_y = -0.25) +
#   xlim(1, 3.6)
#
# ggplot(rate_per_borough, aes(km_cycled, KSI_per_bkm)) +
#   geom_point()

