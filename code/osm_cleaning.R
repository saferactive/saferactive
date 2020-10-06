# osm cleaning
remotes::install_github("itsleeds/osmextract")
library(osmextract)
library(dplyr)
library(progress)
library(tmap)
library(sf)
library(stats19)
tmap_mode("view")
source("code/osm_cleaning_functions.R")

# Read in PBF
osm = oe_read("E:/OneDrive - University of Leeds/Data/opentripplanner/graphs/london/greater-london-latest.osm.pbf",
                    extra_tags = c("ref", "maxspeed"))

# Clean and prepare
osm <- osm_main_roads(osm)
osm <- sf::st_transform(osm, 27700)
osm <- osm_consolidate(osm)
junctions <- osm_get_junctions(osm)
junction_clusters <- cluster_junction(junctions)

# Bring in Stats19 at
crash <- readRDS("ac10.Rds")
crash <- crash[crash$police_force %in% c("Metropolitan Police","City of London"), ]
crash <- crash[!is.na(crash$longitude),] #TODO: What to do with crashes with no location?
crash <- st_as_sf(crash, coords = c("longitude","latitude"), crs = 4326)
crash <- st_transform(crash, 27700)

# Match Crash to road and junction
crash_road <- crash[crash$junction_detail %in% c("Not at junction or within 20 metres",
                                                 "Private drive or entrance"),]
crash_junction <- crash[!crash$junction_detail %in% c("Not at junction or within 20 metres",
                                                 "Private drive or entrance"),]
rm(crash)

# Need nearest line not nearest centroid of line
crash_road_nn <- nn_line(crash_road, osm, ncores = 5)
crash_junction_nn <- nn_point(crash_junction, junctions, clusters = junction_clusters$junction_ids)

crash_road$road_idx <- crash_road_nn$idx
crash_road$road_dist <- crash_road_nn$dist

crash_junction$junction_idx <- crash_junction_nn$cluster_index
crash_junction$junction_dist <- crash_junction_nn$distance

saveRDS(crash_road,"crash_road.Rds")
saveRDS(crash_junction,"crash_junction.Rds")
saveRDS(osm,"roads_london.Rds")
saveRDS(junctions,"junction_london.Rds")

#TODO: What to do with crashed that have large match distance? Usually, A crash
#in off road location e.g. private land, or B, crash with incorrect location
#e.g. in a lake

crash_road_summary <- crash_road %>%
  sf::st_drop_geometry() %>%
  group_by(road_idx) %>%
  summarise(number_of_casualties = sum(number_of_casualties))

crash_junctions_summary <- crash_junction %>%
  sf::st_drop_geometry() %>%
  group_by(junction_idx) %>%
  summarise(number_of_casualties = sum(number_of_casualties))

osm$id <- 1:nrow(osm)
junctions <- st_sf(data.frame(id = 1:length(junctions),
                              geometry = junctions), crs = 27700)


osm <- left_join(osm, crash_road_summary, by = c("id" = "road_idx"))
junctions <- left_join(junctions, crash_junctions_summary, by = c("id" = "junction_idx"))

head(osm)
summary(osm$number_of_casualties)
summary(junctions$number_of_casualties)

osm_cas <- osm[!is.na(osm$number_of_casualties),]
junctions_cas <- junctions[!is.na(junctions$number_of_casualties),]
junctions_cas_point <- st_centroid(junctions_cas)

#Overlay top crashes areas
tm_shape(osm_cas[osm_cas$number_of_casualties > 3,]) +
  tm_lines(col = "number_of_casualties",
           lwd = 3,
           breaks = c(1,5,10,20,50,210),
           palette = "YlOrRd") +

tm_shape(junctions_cas_point[junctions_cas_point$number_of_casualties > 3,]) +
  tm_dots(col = "number_of_casualties",
          breaks = c(1,5,10,20,50,210),
          palette = "YlOrRd",
          size = 0.03)



st_write(st_transform(osm_cas, 4326), "london_road_cas.geojson", delete_dsn = TRUE)
st_write(st_transform(junctions_cas_point, 4326), "london_junction_point_cas.geojson", delete_dsn = TRUE)
