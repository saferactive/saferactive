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
crash_road$nn <- nn_line2(crash_road, osm, ncores = 4)
crash_junction$nn <- nn_line2(crash_junction, junctions, ncores = 4)

crash_road_summary <- crash_road %>%
  sf::st_drop_geometry() %>%
  group_by(nn) %>%
  summarise(number_of_casualties = sum(number_of_casualties))

crash_junctions_summary <- crash_junction %>%
  sf::st_drop_geometry() %>%
  group_by(nn) %>%
  summarise(number_of_casualties = sum(number_of_casualties))

osm$id <- 1:nrow(osm)
junctions <- st_sf(data.frame(id = 1:length(junctions),
                              geometry = junctions), crs = 27700)


osm <- left_join(osm, crash_road_summary, by = c("id" = "nn"))
junctions <- left_join(junctions, crash_junctions_summary, by = c("id" = "nn"))

head(osm)
summary(osm$number_of_casualties)
summary(junctions$number_of_casualties)

osm_cas <- osm[!is.na(osm$number_of_casualties),]
junctions_cas <- junctions[!is.na(junctions$number_of_casualties),]

tm_shape(osm_cas) +
  tm_lines(col = "number_of_casualties",
           lwd = 3,
           breaks = c(0,1,3,6,13,28),
           palette = "viridis") +
  tm_shape(junctions_cas) +
  tm_fill(col = "number_of_casualties",
          breaks = c(0,1,3,6,13,28),
          palette = "viridis")


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
