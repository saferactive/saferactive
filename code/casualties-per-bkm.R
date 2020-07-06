# Get pct commute data for London. Ensure journeys to/from outside London are included
library(pct)
library(tidyverse)
library(mapview)
library(sf)
library(units)
library(hms)
library(lubridate)

# Route journeys to work.
r = pct::get_pct_rnet(region = "london")

r = r %>%
  filter(bicycle > 0)
# mapview(r)

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

summed = c_london %>%
  select(Name, km_cycled) %>%
  group_by(Name) %>%
  summarise(km_cycled = sum(km_cycled)) %>%
  st_drop_geometry()


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
              time_est < hms::as_hms('18:30:00')))

# changed to use adjusted casualty severity, not accident severity
crashes_per_borough = crashes_peak %>%
  group_by(local_authority_district) %>%
  summarise(cycle_fatal = sum(cycle_fatal),
            cycle_adjusted_serious = sum(cycle_adjusted_serious),
            cycle_adjusted_slight = sum(cycle_adjusted_slight)) %>%
  st_drop_geometry()

# Calculate casualties per billion km
# the km cycled is from a single day but the crashes are from five years
# would it be better to pick out weekdays only?
# what about seasonal changes in crash rates?
# For smaller geographic areas, we will have to use all crashes not KSI

rate_per_borough = inner_join(crashes_per_borough, summed, by = c("local_authority_district" = "Name")) %>%
  mutate(fatal_per_bkm = (cycle_fatal/365/5/2)/(km_cycled/1000000000),
         serious_per_bkm = (cycle_adjusted_serious/365/5/2)/(km_cycled/1000000000),
         slight_per_bkm = (cycle_adjusted_slight/365/5/2)/(km_cycled/1000000000),
         KSI_per_bkm = ((cycle_fatal/365/5/2)+(cycle_adjusted_serious/365/5/2))/(km_cycled/1000000000))

rate_per_borough = inner_join(lads, rate_per_borough, by = c("Name" = "local_authority_district")) %>%
  select(-Level)

View(rate_per_borough)

plot(rate_per_borough)

write_rds(rate_per_borough, "rate_per_borough.Rds")

library(tmap)
tmap_mode("view")

tm_shape(rate_per_borough) +
  tm_polygons("KSI_per_bkm")

mapview(rate_per_borough["KSI_per_bkm"])

plot(rate_per_borough$KSI_per_bkm ~ rate_per_borough$km_cycled) # strong exponential decay curve
