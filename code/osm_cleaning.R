# osm cleaning
remotes::install_github("itsleeds/osmextract")
library(osmextract)
library(dplyr)
library(progress)
library(tmap)
library(sf)
tmap_mode("view")
#osm_lines = oe_get("Greater London", extra_tags = c("ref", "maxspeed"))
osm_lines = oe_read("E:/OneDrive - University of Leeds/wheretolive/data/otp/graphs/wy/wy.osm.pbf",
                    extra_tags = c("ref", "maxspeed"))
# names(osm_lines)
# waterloo = osm_lines %>%
#   filter(name == "Waterloo Bridge")
#
# waterloo_buffer = waterloo %>%
#   sf::st_union() %>%
#   stplanr::geo_buffer(dist = 5000)



# Function to subset to main roads in osm
osm_main_roads <- function(x){
  x <- x[!is.na(x$highway),]
  x <- x[x$highway %in% c("primary","primary_link",
                                "secondary","secondary_link",
                                "tertiary","tertiary_link",
                                "trunk","trunk_link",
                                "motorway","motorway_link",
                                "unclassified","residential",
                                "road","mini_roundabout"),]
  x
}

osm_consolidate <- function(x){
  x <- dplyr::group_by(x, name, ref)
  x <- dplyr::summarise(x)
  xls <- x[sf::st_geometry_type(x) == "LINESTRING",]
  xmls <- x[sf::st_geometry_type(x) == "MULTILINESTRING",]
  xmls <- sf::st_line_merge(xmls)
  x <- rbind(xls,xmls)
  x

}

osm_get_junctions <- function(x){
  points <- sf::st_cast(x,"MULTIPOINT")
  points <- points$geometry
  points <- sf::st_cast(points,"POINT")
  # TO be a junction their must be duplication of points
  dup <- duplicated(points)
  points <- points[dup]
  # But we only want on version of the junction
  dup <- duplicated(points)
  points <- points[!dup]
  return(points)
}

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

cluster_junction <- function(x){
  buff <- sf::st_buffer(x, dist = 15, nQuadSegs = 15)
  ints <- sf::st_intersects(buff)
  message("Clustering Junctions")
  ints_clus <- cluster_ints(ints)
  ints_clus <- ints_clus[lengths(ints_clus) > 0]

  message("Creating Geometries")
  geoms <- list()
  pb <- progress_bar$new(total = length(ints_clus))
  for(i in seq_len(length(ints_clus))){
    pb$tick()
    sub <- buff[ints_clus[[i]]]
    if(length(sub) == 1){
      geoms[[i]] <- sub[[1]]
    } else {
      geoms[[i]] <- sf::st_union(sub)[[1]]
    }

  }
  attributes(geoms) <- attributes(buff)
  return(geoms)
}


# bench::mark(simp = sf::st_union(buff[ints_clus[[3]]]),
#             comp = {if(length(ints_clus[[3]]) == 1){
#               buff[ints_clus[[3]]]
#             }})



cluster_ints <- function(x){
  res <- list()
  pb <- progress_bar$new(total = length(x))
  for(i in seq_len(length(x))){
    pb$tick()
    if(length(x[[i]]) == 1){
      if(!is.na(x[[i]])){
        # Single Junction
        res[[i]] <- x[[i]]
      }
      # Else move on
    } else {
      # Multi-Junction Cluster
      sub <- recursive_ints(x[[i]], x = x)
      res[[i]] <- sub
      x[sub] <- NA
    }
  }
  return(res)
}





recursive_ints <- function(sub, x){
  sub <- unique(sub)
  sub <- sub[order(sub)]

  sub2 <- unique(unlist(x[sub]))
  sub2 <- sub2[order(sub2)]

  if(identical(sub, sub2)){
    return(sub)
  } else {
    return(recursive_ints(sub2, x = x))
  }

}


osm <- osm_main_roads(osm_lines)
osm <- sf::st_transform(osm, 27700)
osm <- osm_consolidate(osm)
junctions <- osm_get_junctions(osm)
junctions <- cluster_junction(junctions)


stats19::dl_stats19(2018, type = "Accidents")
crash <- stats19::get_stats19(2018, type = "Accidents", output_format = "sf")
crash <- crash[crash$police_force == "West Yorkshire", ]


# Match Crash to road and junction


crash_road <- crash[crash$junction_detail %in% c("Not at junction or within 20 metres",
                                                 "Private drive or entrance"),]
crash_junction <- crash[!crash$junction_detail %in% c("Not at junction or within 20 metres",
                                                 "Private drive or entrance"),]

# Need nearest line not heares centroid of line


nn_line <- function(point, lines){
  cents <- sf::st_centroid(lines)
  nn <- nngeo::st_nn(point, cents, k = 20)

  res <- list()
  pb <- progress_bar$new(total = nrow(point))
  for(i in seq_len(nrow(point))){
    pb$tick()
    nnsub <- nn[[i]]
    suppressMessages(sub <- unlist(nngeo::st_nn(point$geometry[i], sf::st_geometry(lines)[nnsub], progress = FALSE)))
    res[[i]] <- nnsub[sub]
  }
  res <- unlist(res)
  return(res)

}

#system.time(bar <- nn_line(crash_road[1:2,], osm)) # fast
#system.time(foo <- nngeo::st_nn(crash_road[1:2], osm)) # slow

crash_road_nn <- nn_line(crash_road, osm)
crash_junction_nn <- nn_line(crash_junction, junctions)

crash_road$nn <- crash_road_nn
crash_junction$nn <- crash_junction_nn

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


