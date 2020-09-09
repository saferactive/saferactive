# osm cleaning
remotes::install_github("itsleeds/osmextract")
library(osmextract)
library(dplyr)
osm_lines = oe_get("Greater London", extra_tags = c("ref", "maxspeed"))
names(osm_lines)
waterloo = osm_lines %>%
  filter(name == "Waterloo Bridge")

waterloo_buffer = waterloo %>%
  sf::st_union() %>%
  stplanr::geo_buffer(dist = 1000)



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
  points <- points[duplicated(points)]
  points <- points[!duplicated(points)]
  points
}

# TODP: TRY BUFFER AND UNION
cluster_junction <- function(x){
  buff <- sf::st_buffer(x, 15)
  ints <- sf::st_intersects(buff)
  ints_lths <- lengths(ints)
  buff_single <- buff[lengths(ints) == 1]
  buff_many <- buff[lengths(ints) > 1]
  buff_many <- sf::st_buffer(sf::st_union(buff_many), 2)
  buff_many <- sf::st_cast(buff_many, "POLYGON")
  buff_many <- sf::st_simplify(buff_many, dTolerance = 5)
  res <- c(buff_single, buff_many)
  return(res)
  mapview::mapview(buff_many)

  # nn <- nngeo::st_nn(x, x, maxdist = 50, k = length(x))
  # nn <- nn[lengths(nn) > 3]
  #
  # foo <- x[unique(unlist(nn))]
  # mapview::mapview(foo)
  # points <- leaderCluster::leaderCluster(sf::st_coordinates(sf::st_transform(foo, 4326)), radius = 0.1, distance = "haversine")


  # clust <- points$cluster_centroids
  # clust <- as.data.frame(clust)
  # names(clust) <- c("X","Y")
  # clust <- sf::st_as_sf(clust, coords = c("X","Y"), crs = 4326)
  # clust <- sf::st_buffer(clust, 0.0005)
  #
  # clust <- sf::st_sf(data.frame(id = points$cluster_id, geometry = foo), crs = 27700, stringsAsFactors = FALSE)
  # clust_count <- clust$id[duplicated(clust$id)]
  # clust_one <- clust[!clust$id %in% clust_count,]
  # clust_many <- clust[clust$id %in% clust_count,]
  #
  # clust_many <- dplyr::group_by(clust_many, id)
  # clust_many <- dplyr::summarise(clust_many)
  # clust_many <- sf::st_convex_hull(clust_many)
  # clust_many <- sf::st_buffer(clust_many, 10)
  # mapview::mapview(clust_many)
  #
  # mapview::mapview(clust) + mapview::mapview(junctions, color = "red")
}


osm_case_study = osm_lines[waterloo_buffer, , op = sf::st_within]
mapview::mapview(osm_case_study)

osm_case_study <- sf::st_transform(osm_case_study, 27700)
osm_case_study <- osm_main_roads(osm_case_study)
osm_case_study <- osm_consolidate(osm_case_study)
junctions <- osm_get_junctions(osm_case_study)
junctions <- cluster_junction(junctions)
mapview::mapview(osm_case_study) + mapview::mapview(junctions)


