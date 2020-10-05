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
osm_lines = oe_read("E:/OneDrive - University of Leeds/Data/opentripplanner/graphs/london/greater-london-latest.osm.pbf",
                    extra_tags = c("ref", "maxspeed"))

# Clean and prepare
osm <- osm_main_roads(osm_lines)
osm <- sf::st_transform(osm, 27700)
osm <- osm_consolidate(osm)
junctions <- osm_get_junctions(osm)
junctions <- cluster_junction(junctions)
rm(osm_lines)

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
crash_road_nn <- nn_line(crash_road, osm, ncores = 4)
crash_junction_nn <- nn_line(crash_junction, junctions, ncores = 4)

crash_road$road_idx <- crash_road_nn$idx
crash_road$road_dist <- crash_road_nn$dist

crash_junction$junction_idx <- crash_junction_nn$idx
crash_junction$junction_dist <- crash_junction_nn$dist

saveRDS(crash_road,"crash_road.Rds")
saveRDS(crash_junction,"crash_junction.Rds")
saveRDS(osm,"roads_london.Rds")
saveRDS(junctions,"junction_london.Rds")

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

tm_shape(osm_cas) +
  tm_lines(col = "number_of_casualties",
           lwd = 3,
           breaks = c(1,5,10,20,50,130),
           palette = "viridis")

tm_shape(junctions_cas_point) +
tm_dots(col = "number_of_casualties",
        breaks = c(1,5,10,20,50,210),
        palette = "viridis")


#Overlay top crashes areas

tm_shape(osm_cas[osm_cas$number_of_casualties > 2,]) +
  tm_lines(col = "number_of_casualties",
           lwd = 3,
           breaks = c(1,5,10,20,50,210),
           palette = "viridis") +

tm_shape(junctions_cas_point[junctions_cas_point$number_of_casualties > 2,]) +
  tm_dots(col = "number_of_casualties",
          breaks = c(1,5,10,20,50,210),
          palette = "viridis")

message(Sys.time())

# Old code ----------------------------------------------------------------

stop("Old Code please ignore")

# TODP: TRY BUFFER AND UNION
# cluster_junction <- function(x){
#   buff <- sf::st_buffer(x, 15)
#   ints <- sf::st_intersects(buff)
#   ints_lths <- lengths(ints)
#   buff_single <- buff[lengths(ints) == 1]
#   buff_many <- buff[lengths(ints) > 1]
#   buff_many <- sf::st_buffer(sf::st_union(buff_many), 2)
#   buff_many <- sf::st_cast(buff_many, "POLYGON")
#   buff_many <- sf::st_simplify(buff_many, dTolerance = 5)
#   res <- c(buff_single, buff_many)
#   return(res)
# }


# bench::mark(simp = sf::st_union(buff[ints_clus[[3]]]),
#             comp = {if(length(ints_clus[[3]]) == 1){
#               buff[ints_clus[[3]]]
#             }})


#system.time(bar <- nn_line(crash_road[1:2,], osm)) # fast
#system.time(foo <- nngeo::st_nn(crash_road[1:2], osm)) # slow


# foo = nn_line2(crash_road[1:100,], osm, ncores = 4)
# foo2 = nn_line2(crash_road[1:100,], osm, ncores = 4, k = 100)
# qtm(crash_road[1:100,]) +
#   qtm(osm[foo,])
#
# qtm(crash_road[1:100,]) +
#   qtm(osm[foo2,])
