library(tidyverse)
library(sf)
library(mapview)

u = "http://data.dft.gov.uk/road-traffic/dft_traffic_counts_raw_counts.zip"
download.file(u, "./dft_traffic_counts_raw_counts-2000-2018.zip")
traffic_data_original = readr::read_csv("./dft_traffic_counts_raw_counts-2000-2018.zip")

names(traffic_data_original)

counter_16598 = traffic_data_original %>%
  filter(count_point_id == "16598")

counter_16598 %>%
  filter(year == 2016) %>%
  summarise(pedal_cycles = sum(pedal_cycles)) # 326 why not 366???
counter_16598$link_length_km

##London Boroughs

lads = readRDS("lads.Rds")
boroughs = as.character(spData::lnd$NAME)
lads = lads %>%
  filter(Name %in% boroughs)

##Counters per London Borough
counters_la = readr::read_csv("http://data.dft.gov.uk/road-traffic/local_authority_traffic.csv")
counters_lnd = counters_la %>%
  filter(name %in% boroughs)
dim(counters_lnd)

# lads = ukboundaries::lad2018
# summary(lads$Name %in% counters_la$name)
# summary(lads$lau118cd %in% counters_la$ons_code)
# length(unique(counters_la$name))
# length(unique(lads$lau118nm))

## All traffic count data for London
traffic_london = traffic_data_original %>%
  filter(local_authority_name %in% boroughs)

max(traffic_london$easting)
min(traffic_london$easting)
max(traffic_london$northing)
min(traffic_london$northing)

traffic_london = traffic_london %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700)

saveRDS(traffic_london, "traffic_london.Rds")

traffic_london = readRDS("traffic_london.Rds")

# there are different numbers of traffic count points for each year. I think this includes only the actual counts, not the intermediate years where estimates were made.
traffic_london %>%
  st_drop_geometry() %>%
  count(year)

###Select peak hours only, sum counts by year and direction of travel (could remove direction of travel since PCT data doesn't use this)
traffic_london_points = traffic_london %>%
  filter(hour == c(7,8,9,16,17,18)) %>%
  group_by(count_point_id,
           # direction_of_travel,
           year) %>%
  summarise(pedal_cycles = sum(pedal_cycles))

traffic_2011 = traffic_london_points %>%
  filter(year == 2011)


# ttop = top_frac(traffic_london, 0.1, wt = pedal_cycles)
# mapview::mapview(ttop)

##Find road_name (ref)
glimpse(traffic_london_points)

dim(traffic_london_points)
traffic_london_points$road_name




# Join traffic counts with PCT rnet ---------------------------------------


##Join the traffic counts with PCT route network segments. We should join nearest neighbours (using segment centroids) that have the same road ref. The PCT rnet doesn't include road ref. For the rapid tool we got the road refs (along with road width) from the cyipt data (rj.Rds). We could do the same again, buffering etc to deal with issues of differing road networks. Or it may be easier to join with up-to-date OSM data instead.

# we need to get road names/refs for the PCT data, to be sure the right roads are getting joined, not just the nearest ones.

# # this code joins without using road refs
# pct_cent = readRDS("cent.Rds") %>%
#   st_transform(27700)
#
# join_by_dist = st_join(traffic_london_points, pct_cent, join = st_nearest_feature)
#
# join_2011 = join_by_dist %>%
#   filter(year == 2011)
# plot(join_2011$pedal_cycles ~ join_2011$bicycle)





# Aggregate by Borough ----------------------------------------------------

traffic_boroughs = traffic_london_points %>%
  group_by(local_authority_name, year) %>%
  summarise(pedal_cycles = mean(pedal_cycles)) %>%
  st_drop_geometry

traffic_boroughs = inner_join(lads, traffic_boroughs, by = c("Name" = "local_authority_name"))

mapview::mapview(traffic_boroughs["pedal_cycles"])

# ## Aggregate by MSOA??
# traffic_boroughs = traffic_london_points %>%
#   group_by(local_authority_name, year) %>%
#   summarise(pedal_cycles = mean(pedal_cycles)) %>%
#   st_drop_geometry
#
# traffic_boroughs = inner_join(lads, traffic_boroughs, by = c("Name" = "local_authority_name"))
#
# mapview::mapview(traffic_boroughs["pedal_cycles"])





# Raster grid showing cycle count data ------------------------------------

#issues:
## 1) there are not many points from each year. I could extrapolate points to the years between surveys to get more points for each year, and so make the raster accurately reflect spatial trends
## 2) do a similar raster for the pct rnet
## 3) use spatial interpolation to make the raster continuous? that will be needed for a higher resolution grid e.g. 500m.



library(raster)

raster_template = raster(extent(traffic_london_points), resolution = 1000, crs = st_crs(traffic_london_points)$proj4string)

rast_counts = rasterize(x = traffic_london_points, y = raster_template, field = "pedal_cycles", fun = mean)

mapview(lads, alpha.regions = 0.1) + mapview(rast_counts)

# library(tmap)
# tmap_mode("view")
#
# tm_shape(rast_counts) +
#   tm_raster() +
#   tm_facets(as.layers = TRUE)



# dist_rast = distanceFromPoints(raster_template,traffic_london_points)
# mapview(dist_rast)

library(spatstat)
rast_val = idw(X = trformula = pedal_cycles ~ 1,
    locations = traffic_london_points,
    newdata = raster_lnd,
    idp = 3)


