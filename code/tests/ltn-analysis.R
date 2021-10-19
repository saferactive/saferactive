# Aim: get data from cyclestreets on ltns

remotes::install_github("cyclestreets/cyclestreets-r")
library(cyclestreets)
library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")
library(mapview)

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


# Get West Yorkshire definition
lad2018 = ukboundaries::lad2018
centroid_lad2018 = lad2018 %>%
  st_centroid()
# grid_leeds = grid_ew[ukboundaries::leeds,]

westyorks = lad2018 %>%
  filter(lau118nm %in% c("Leeds", "Bradford", "Wakefield", "Kirklees", "Calderdale")) # These boundaries are more accurate

# Get national grid
grid_ew = sf::read_sf("https://github.com/charlesroper/OSGB_Grids/raw/master/GeoJSON/OSGB_Grid_1km.geojson")
length(grid_ew$geometry) # 286000
# [1] 336168
grid_ew_centroids = sf::st_centroid(grid_ew)

cent_westyorks = grid_ew_centroids[westyorks, ]
grid_westyorks = grid_ew[cent_westyorks, ] %>%
  st_transform(27700)


# get LTN data on 1km grid for all PCT regions...
regions = pct::pct_regions
i = "isle-of-wight"
region_names_ordered = regions$region_name[c(44:1, 45)]

# for(i in region_names_ordered[1:length(region_names_ordered)]) {
#   message("building for ", i)
#   z = regions[regions$region_name == i, ] #These boundaries are supergeneralised, meaning they miss Knottingley and other bits
#   z2 = st_join(z, centroid_lad2018)
#   z3 = lad2018[lad2018$lau118cd %in% z2$lau118cd, ] # These boundaries are more accurate
#   gzc = grid_ew_centroids[z3, ]
#   gz = grid_ew[gzc, ]
#   # qtm(z3) + qtm(gz)
#   ltn_data = purrr::map_dfr(gz$geometry, cyclestreets::ltns)
#   ltn_data = distinct(ltn_data)
#   f = paste0("lnt_data_", i, ".Rds")
#   # plot(ltn_data["ratrun"])
#   saveRDS(ltn_data, f)
#   piggyback::pb_upload(f)
# }

# get more accurate grid for each region (as long as centroids are within regions - may not work for western isles / isles of scilly)
for(i in region_names_ordered) {
  message("building for ", i)
  z = regions[regions$region_name == i, ] #These boundaries are supergeneralised, meaning they miss Knottingley and other bits
  z2 = st_join(z, centroid_lad2018)
  z3 = lad2018[lad2018$lau118cd %in% z2$lau118cd, ] # These boundaries are more accurate
  gzc = grid_ew_centroids[z3, ]
  gz = grid_ew[gzc, ]
  gz = gz %>% st_transform(27700)
  for_name = gsub(x = i, pattern = "-", replacement = "_")
  assign(paste0("grid_", for_name), gz)
  f = paste0("grid_", for_name)
  saveRDS(gz, f)
  # piggyback::pb_upload(f)
}


# Clean and combine data --------------------------------------------------


for(i in region_names_ordered) {
  message("building for ", i)
  f = paste0("lnt_data_", i, ".Rds")
  ltn_data = readRDS(f)
  ltn_data_unique = distinct(ltn_data)
  nrow(ltn_data_unique) / nrow(ltn_data) # more than 10% duplicates in iow
  f = paste0("lnt_data_distinct", i, ".Rds")
  # piggyback::pb_delete(f)
  # plot(ltn_data["ratrun"])
  saveRDS(ltn_data_unique, f)
  # piggyback::pb_upload(f)
}

# f_all = paste0("lnt_data_distinct", region_names_ordered, ".Rds")[1:3] # for testing
f_all = paste0("lnt_data_distinct", region_names_ordered, ".Rds")

ltn_all = purrr::map_dfr(f_all, readRDS)
saveRDS(ltn_all, "ltn_all_distinct_wgs84.Rds")
sf::write_sf(ltn_all, "ltn_all_distinct_wgs84.gpkg")
# then transform in qgis...
# docker run -d -p 8785:8787 -e DISABLE_AUTH=TRUE -v $(pwd):/home/rstudio/geocompr  geocompr/geocompr:qgis
library(sf)
browseURL("http://localhost:8785/")
remotes::install_github("paleolimbot/qgisprocess")
library(qgisprocess)
qgisprocess::qgis_configure()
# rnet_projected = sf::st_transform(rnet, 27700)
rnet_projected = sf::read_sf("ltn_all_distinct_27700.gpkg")
res = qgis_run_algorithm(
  algorithm = "grass7:v.split",
  input = rnet_projected,
  length = 500
)

rnet_split_qgis = sf::read_sf(res$output)
nrow(rnet_split_qgis) / nrow(rnet_projected)
summary(sf::st_length(rnet_split_qgis))
saveRDS(rnet_split_qgis, "ltn_all_distinct_27700_split.Rds")
# in system CLI:


# # tests:
# nrow(ltn_all) # 3.1m segments
# ltn_all %>%
#   sample_n(10000) %>%
#   qtm()
# class(ltn_all)
# saveRDS(ltn_all, "~/repos/stplanr/ltn_all.Rds")
# summary(sf::st_length(ltn_all))
# # Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# # 0.693   31.899   65.432  159.348  153.453 8062.397
ltn_data = st_transform(ltn_data, 27700)


# New method using split LTN data -----------------------------------------

# Read in LTN data using new method

# Previous version with separate files for each pct region
# ltn_data = readRDS(url("https://github.com/saferactive/saferactive/releases/download/0.1.4/lnt_data_west-yorkshire.Rds"))
# ltn_data = distinct(ltn_data)
# ltn_data = ltn_data %>%
#   mutate(length = units::drop_units(st_length(ltn_data)))
# saveRDS(ltn_data, "ltn_updated_west-yorkshire.Rds")

# backup sanity check (not split)
# ltn_data = readRDS("ltn_all_distinct_wgs84.Rds")

# correct version
ltn_data = readRDS("ltn_all_distinct_27700_split.Rds")
st_crs(ltn_data) = 27700
# mapview::mapview(sample_n(ltn_data, 1000))

# Remove motorways
# Can now do this for whole country
# ltn_data = ltn_data %>%
#   filter(! name %in% c("M62", "M1", "A1(M)", "M621", "M606"))
ltn_data = ltn_data[! grepl("M[123456789]", x = ltn_data$name), ]
ltn_data = ltn_data[! grepl("\\(M)", x = ltn_data$name), ]

ltn_data = ltn_data %>%
  mutate(length = units::drop_units(st_length(ltn_data)))

# Find centroid of each segment
ltn_centroid = st_centroid(ltn_data)

saveRDS(ltn_centroid, "ltn_centroid.Rds")

ltn_centroid = readRDS("ltn_centroid.Rds")

# Do it for west yorkshire (or other regions)
region = "west_yorkshire"
grid_region = get(paste0("grid_", region))

# Aggregate LTN data using spatial grid
ltn_westyorks = st_join(ltn_centroid, grid_region)
ltn_westyorks = ltn_westyorks[! is.na(ltn_westyorks$PLAN_NO), ]

ltn_aggregated = ltn_westyorks %>%
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
crashes = crashes %>%
  st_transform(27700)
crashes_westyorks = crashes[grid_region, ]

grid_crashes = st_join(crashes_westyorks, grid_region)

crashes_aggregated = grid_crashes %>%
  st_drop_geometry() %>%
  group_by(PLAN_NO) %>%
  summarise(
    ksi_cycle = sum(casualty_serious_cyclist) + sum(casualty_fatal_cyclist),
    ksi_walk = sum(casualty_serious_pedestrian) + sum(casualty_fatal_pedestrian)
  )

# Get PCT rnet and aggregate cycle flows to grid
# Need to split the rnet to avoid segments >1km in length
# pct_rnet = pct::get_pct_rnet(region = "west-yorkshire", geography = "lsoa", purpose = "commute")
rnet_split = st_read("rnet_split.gpkg")

rnet_split = rnet_split %>%
  # st_transform(27700) %>%
  mutate(
    length = units::drop_units(st_length(rnet_split)),
    cycle_km = bicycle * length / 1000
  )

rnet_centroids = st_centroid(rnet_split)
saveRDS(rnet_centroids, "rnet_centroids.Rds")

rnet_centroids = readRDS("rnet_centroids.Rds")

pct_westyorks = st_join(rnet_centroids, grid_region)
pct_westyorks = pct_westyorks[! is.na(pct_westyorks$PLAN_NO), ]

pct_aggregated = pct_westyorks %>%
  st_drop_geometry() %>%
  group_by(PLAN_NO) %>%
  summarise(
   cycle_km = sum(cycle_km)
  )

# Join LTN and crash data with grid (the left joins ensure only grid cells within west yorkshire are included)
ltn_grid = left_join(grid_region, ltn_aggregated)
ltn_grid = left_join(ltn_grid, crashes_aggregated)
ltn_grid = left_join(ltn_grid, pct_aggregated)

ltn_grid$ksi_cycle[is.na(ltn_grid$ksi_cycle)] = 0
ltn_grid$ksi_walk[is.na(ltn_grid$ksi_walk)] = 0
ltn_grid$cycle_km[is.na(ltn_grid$cycle_km)] = 0 # prevent NaN

ltn_grid$cycle_km_cleaned = case_when(
  ltn_grid$cycle_km >= 1 ~ ltn_grid$cycle_km,
  TRUE ~ 1
  ) # prevent NaN

ltn_grid = ltn_grid %>%
  mutate(
    cycle_ksi_per_km_road = ksi_cycle / length_total * 1000,
    walk_ksi_per_km_road = ksi_walk / length_total * 1000,
    cycle_ksi_per_km_cycled = ksi_cycle / cycle_km_cleaned,
    cycle_ksi_per_Bkm_cycled = cycle_ksi_per_km_cycled/255/2/10*1000000000*0.35 # pct rnet flows are uni-directional only and need to be multiplied by the number of working days per year. KSI data cover 10 years. In 2010-2019, according to NTS table 0409 a mean of 35% of cycle journeys were for commuting
    )

# Grid only including cells that contain roads
ltn_grid_full = ltn_grid[! is.na(ltn_grid$length_total), ]

# Grid only including cells with >= 1km cycling on PCT rnet
ltn_grid_ck = ltn_grid_full[ltn_grid_full$cycle_km >= 1, ]

# change to tmap with log scales for better discrimination
plot(ltn_grid)
mapview(ltn_grid["length_total"])
mapview(ltn_grid["length_main"])
mapview(ltn_grid["perc_main"])
mapview(ltn_grid["perc_ratrun"])
mapview(ltn_grid["perc_calmed"])
mapview(ltn_grid["perc_ltn"])
mapview(ltn_grid["ksi_cycle"])
mapview(ltn_grid["ksi_walk"])
mapview(ltn_grid_ck["cycle_km_cleaned"])
mapview(ltn_grid["cycle_ksi_per_km_road"])
mapview(ltn_grid["walk_ksi_per_km_road"])
mapview(ltn_grid_ck["cycle_ksi_per_km_cycled"])

a = tm_shape(ltn_grid_full) +
  tm_fill("length_total", title = "Road length", alpha = 0.8)
b = tm_shape(ltn_grid_full) +
  tm_fill("perc_main", title = "% main roads", alpha = 0.8)
c = tm_shape(ltn_grid_full) +
  tm_fill("perc_ltn", title = "% LTN", alpha = 0.8)
d = tm_shape(ltn_grid_full) +
  tm_fill("perc_ratrun", title = "% ratrun", alpha = 0.8)
e = tm_shape(ltn_grid_full) +
  tm_fill("perc_calmed", title = "% calmed ratrun", alpha = 0.8
          , breaks = c(0, 0.02, 0.05, 0.1, 0.2, 1))
tmap_arrange(a, b, c, d, e, ncol = 5, nrow = 1)


a = tm_shape(ltn_grid_full) +
  tm_fill("ksi_cycle", title = "Cycle KSI", alpha = 0.8
          , breaks = c(0, 1, 2, 5, 10, 30))
b = tm_shape(ltn_grid_full) +
  tm_fill("ksi_walk", title = "Walk KSI", alpha = 0.8
          , breaks = c(0, 5, 10, 20, 50, 120)
          )
c = tm_shape(ltn_grid_ck) +
  tm_fill("cycle_km_cleaned", title = "Cycle commute km", alpha = 0.8
          , breaks = c(0, 50, 100, 200, 500, 1000)
  )
d = tm_shape(ltn_grid_full) +
  tm_fill("cycle_ksi_per_km_road", title = "Cycle KSI per km road", alpha = 0.8
          , breaks = c(0, 0.05, 0.1, 0.2, 0.5, 2)
  )
e = tm_shape(ltn_grid_full) +
  tm_fill("walk_ksi_per_km_road", title = "Walk KSI per km road", alpha = 0.8
          , breaks = c(0, 0.1, 0.2, 0.5, 1, 5)
  )
f = tm_shape(ltn_grid_ck) +
  tm_fill("cycle_ksi_per_Bkm_cycled", title = "KSI per Bkm cycled", alpha = 0.8
          , breaks = c(0, 2000, 5000, 10000, 50000, 200000)
          )
tmap_arrange(a, b, c, nrow = 1, ncol = 3)
tmap_arrange(d, e, f, nrow = 1, ncol = 3)

ltn_grid_cut = ltn_grid %>%
  filter(length_total > 1000)
ltn_grid_full_cut = ltn_grid_full %>%
  filter(length_total > 1000)
ltn_grid_ck_cut = ltn_grid_ck %>%
  filter(length_total > 1000)

ggplot(ltn_grid, aes(length_total/1000, ksi_walk)) +
  geom_point() +
  labs(x = "Road length (km)", y = "Pedestrian KSI")

ggplot(ltn_grid, aes(length_total/1000 , ksi_cycle)) +
  geom_point() +
  labs(x = "Road length (km)", y = "Cycle KSI")

ggplot(ltn_grid_ck_cut, aes(cycle_km_cleaned , ksi_cycle)) +
  geom_point(alpha = ltn_grid_ck_cut$length_total/max(ltn_grid_ck_cut$length_total)) +
  labs(x = "Km cycled (travel to work)", y = "Cycle KSI")

# Graphs with % ltn
ltn_ck_filter = ltn_grid_ck_cut %>%
  filter(perc_ltn > 0.2, perc_ltn < 0.95)
ltn_full_filter = ltn_grid_full_cut %>%
  filter(perc_ltn > 0.2, perc_ltn < 0.95)

ggplot(ltn_grid_full_cut, aes(perc_ltn, cycle_ksi_per_km_road)) +
  geom_point(alpha = ltn_grid_full_cut$length_total/max(ltn_grid_full_cut$length_total)) +
  labs(x = "% low traffic neighbourhood", y = "Cycle KSI per km road") +
  geom_smooth(method = "glm", mapping = aes(weight = length_total))

ggplot(ltn_grid_ck_cut, aes(perc_ltn, cycle_ksi_per_km_cycled)) +
  geom_point(alpha = ltn_grid_ck_cut$length_total/max(ltn_grid_ck_cut$length_total)) +
  labs(x = "% low traffic neighbourhood", y = "Cycle KSI per km cycled") +
  geom_smooth(method = "glm", mapping = aes(weight = length_total))

ggplot(ltn_grid_full_cut, aes(perc_ltn, walk_ksi_per_km_road)) +
  geom_point(alpha = ltn_grid_full_cut$length_total/max(ltn_grid_full_cut$length_total)) +
  labs(x = "% low traffic neighbourhood", y = "Pedestrian KSI per km road") +
  geom_smooth(method = "glm", mapping = aes(weight = length_total))

# Graphs with % main
main_ck_filter = ltn_grid_ck_cut %>%
  filter(perc_main < 0.5)
main_full_filter = ltn_grid_full_cut %>%
  filter(perc_main < 0.5)

ggplot(ltn_grid_full_cut, aes(perc_ratrun, cycle_ksi_per_km_road)) +
  geom_point(alpha = ltn_grid_full_cut$length_total/max(ltn_grid_full_cut$length_total)) +
  labs(x = "% ratrun", y = "Cycle KSI per km road") +
  geom_smooth(method = "glm", mapping = aes(weight = length_total))

ggplot(ltn_grid_ck_cut, aes(perc_ratrun, cycle_ksi_per_km_cycled)) +
  geom_point(alpha = ltn_grid_ck_cut$length_total/max(ltn_grid_ck_cut$length_total)) +
  labs(x = "% main road", y = "Cycle KSI per km cycled") +
  geom_smooth(method = "glm", mapping = aes(weight = length_total))

ggplot(ltn_grid_full_cut, aes(perc_ratrun, walk_ksi_per_km_road)) +
  geom_point(alpha = ltn_grid_full_cut$length_total/max(ltn_grid_full_cut$length_total)) +
  labs(x = "% rat run", y = "Pedestrian KSI per km road") +
  geom_smooth(method = "glm", mapping = aes(weight = length_total))

