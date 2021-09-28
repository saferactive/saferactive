# Aim: get data from cyclestreets on ltns

remotes::install_github("cyclestreets/cyclestreets-r")
library(cyclestreets)
library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")

# u = "https://api.cyclestreets.net/v2/advocacydata.ltns?bbox=0.101131,52.195807,0.170288,52.209719&zoom=15"
# browseURL(u) # requires api key
#
# # try with API key
# u = paste0("https://api.cyclestreets.net/v2/advocacydata.ltns?key=",
#            Sys.getenv("CYCLESTREETS"),
#            "&bbox=0.101131,52.195807,0.170288,52.209719&zoom=15")
# browseURL(u)
# ltn_data = sf::read_sf(u)
#
# # get data for 3km radius surrounding Leeds
# z = zonebuilder::zb_zone(x = "Leeds", n_circles = 2)
# z = sf::st_transform(z, 27700)
# grid = st_make_grid(z,
#                     what = "polygons",
#                     cellsize = c(500, 500))
# qtm(z) + qtm(grid)
# grid_wgs = sf::st_transform(grid, 4326)
#
# # get for leeds
# ltn_data = cyclestreets::ltns(grid_wgs[1]) # works for one but patchy
# ltn_data = purrr::map_dfr(grid_wgs, cyclestreets::ltns)
# nrow(ltn_data) # 11k
# mapview::mapview(ltn_data["ratrun"])

# get LTN data on 1km grid for all PCT regions...
regions = pct::pct_regions
i = "isle-of-wight"
grid_ew = sf::read_sf("https://github.com/charlesroper/OSGB_Grids/raw/master/GeoJSON/OSGB_Grid_1km.geojson")
length(grid_ew$geometry) # 286000
# [1] 336168
grid_ew_centroids = sf::st_centroid(grid_ew)
region_names_ordered = regions$region_name[c(44:1, 45)]
for(i in region_names_ordered[31:length(region_names_ordered)]) {
  message("building for ", i)
  z = regions[regions$region_name == i, ]
  gzc = grid_ew_centroids[z, ]
  qtm(z) + qtm(gz)
  gz = grid_ew[gzc, ]
  ltn_data = purrr::map_dfr(gz$geometry, cyclestreets::ltns)
  f = paste0("lnt_data_", i, ".Rds")
  # plot(ltn_data["ratrun"])
  saveRDS(ltn_data, f)
  piggyback::pb_upload(f)
}

ltn_data = st_transform(ltn_data, 27700)


ltn_data = readRDS(url("https://github.com/saferactive/saferactive/releases/download/0.1.4/lnt_data_west-yorkshire.Rds"))
# mapview::mapview(sample_n(ltn_data, 1000))

# Remove duplicates and motorways
ltn_data = distinct(ltn_data)
ltn_data = ltn_data %>%
  filter(! name %in% c("M62", "M1", "A1(M)", "M621", "M606"))

# Find the length of each segment
ltn_data = ltn_data %>%
  mutate(length = st_length(ltn_data))

# Split segments that are longer than 500m
# Not needed for now

# Find centroid of each segment
ltn_centroid = st_centroid(ltn_data)

# # Assign segments to a spatial grid
# grid = st_make_grid(ltn_data,
#                     what = "polygons",
#                     cellsize = c(500,500))
#
# grid = st_as_sf(data.frame(gridid = seq(1, length(grid)),
#                            geometry = grid))

# Get West Yorkshire definition
lad2018 = ukboundaries::lad2018
westyorks = lad2018 %>%
  filter(lau118nm %in% c("Leeds", "Bradford", "Wakefield", "Kirklees", "Calderdale"))
# grid_leeds = grid_ew[ukboundaries::leeds,]
grid_westyorks = grid_ew[westyorks,]

# Aggregate LTN data using spatial grid
rnet2 = st_join(ltn_centroid, grid_westyorks)

ltn_aggregated = rnet2 %>%
  st_drop_geometry() %>%
  group_by(PLAN_NO) %>%
  summarise(
    length_ltn = sum(length[which(ratrun == "no")]),
    length_ratrun = sum(length[which(ratrun == "yes")]),
    length_calmed = sum(length[which(ratrun == "calmed")]),
    length_main = sum(length[which(ratrun == "main")]),
    length_total = sum(length),
    perc_ltn = length_ltn/length_total,
    perc_ratrun = length_ratrun/length_total,
    perc_calmed = length_calmed/length_total,
    perc_main = length_main/length_total
  )

# Get and aggregate stats19 data
crashes = readRDS("stats19_2010_2019.Rds")
crashes_westyorks = crashes[westyorks,]

rnet_crashes = st_join(crashes_westyorks, grid_westyorks)

crashes_aggregated = rnet_crashes %>%
  st_drop_geometry() %>%
  group_by(PLAN_NO) %>%
  summarise(
    ksi_cycle = sum(casualty_serious_cyclist) + sum(casualty_fatal_cyclist),
    ksi_walk = sum(casualty_serious_pedestrian) + sum(casualty_fatal_pedestrian)
  )

# Join LTN and crash data with grid
ltn_grid = left_join(grid_westyorks, ltn_aggregated)
ltn_grid = left_join(ltn_grid, crashes_aggregated)

ltn_grid$ksi_cycle[is.na(ltn_grid$ksi_cycle)] = 0
ltn_grid$ksi_walk[is.na(ltn_grid$ksi_walk)] = 0

ltn_grid = ltn_grid %>%
  mutate(
    cycle_ksi_per_km = ksi_cycle / length_total / 1000,
    walk_ksi_per_km = ksi_walk / length_total / 1000
    )

plot(ltn_grid)
mapview(ltn_grid["length_total"])
mapview(ltn_grid["length_main"])
mapview(ltn_grid2["perc_main"])
mapview(ltn_grid["perc_ratrun"])
mapview(ltn_grid["perc_calmed"])
mapview(ltn_grid["perc_ltn"])
mapview(ltn_grid["ksi_cycle"])
mapview(ltn_grid["ksi_walk"])
mapview(ltn_grid["cycle_ksi_per_km"])
mapview(ltn_grid["walk_ksi_per_km"])

ggplot(ltn_grid, aes(units::drop_units(length_total/1000) , ksi_walk)) +
  geom_point() +
  labs(x = "Road length (km)", y = "Pedestrian KSI")

ggplot(ltn_grid, aes(units::drop_units(length_total/1000) , ksi_cycle)) +
  geom_point() +
  labs(x = "Road length (km)", y = "Cycle KSI")

# Get PCT rnet and aggregate cycle flows to grid






croproads <- function(x, y){
  sf::st_crop(x, sf::st_bbox(y))
}



library(furrr)
library(future)
plan(multisession)
rnet_split <- future_map2(.x = rnet2, .y = grid_split, .f = croproads)
plan(sequential)

rnet_split <- bind_rows(rnet_split)
saveRDS(rnet_split, "data/pct_rnet_split_GAM_grid.Rds")

#rnet_split <- readRDS("data/pct_rnet_split_GAM_grid.Rds")

rnet_split$length <- as.numeric(st_length(rnet_split))
rnet_split$km_cycle_2011 <- rnet_split$length / 1000 * rnet_split$bicycle
rnet_split$km_cycle_govtarget <- rnet_split$length / 1000 * rnet_split$govtarget_slc
rnet_split$km_cycle_dutch <- rnet_split$length / 1000 * rnet_split$dutch_slc

rnet_summary <- rnet_split %>%
  st_drop_geometry() %>%
  group_by(gridid) %>%
  summarise(road_km = sum(length / 1000),
            km_cycle_2011 = sum(km_cycle_2011, na.rm = TRUE),
            km_cycle_govtarget = sum(km_cycle_govtarget, na.rm = TRUE),
            km_cycle_dutch = sum(km_cycle_dutch, na.rm = TRUE)
  )



# Assign stats19 crashes to the same spatial grid
