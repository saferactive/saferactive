library(cyclestreets)
library(tidyverse)
library(sf)

u = paste0("https://api.cyclestreets.net/v2/advocacydata.ltns?key=",
           Sys.getenv("CYCLESTREETS"),
           "&bbox=0.101131,52.195807,0.170288,52.209719&zoom=15")
browseURL(u)
ltn_data = sf::read_sf(u)
mapview::mapview(ltn_data["ratrun"])

ltn_data = st_transform(ltn_data, 27700)

# Find the length of each segment and its
ltn_data = ltn_data %>%
  mutate(length = st_length(ltn_data))

# Split segments that are longer than 500m
# Not needed for now


# Find centroid of each segment
ltn_centroid = st_centroid(ltn_data)

# Assign segments to a spatial grid
grid = st_make_grid(ltn_data,
                    what = "polygons",
                    cellsize = c(500,500))

grid = st_as_sf(data.frame(gridid = seq(1, length(grid)),
                           geometry = grid))

rnet2 = st_join(ltn_centroid, grid)
ltn_aggregated = rnet2 %>%
  st_drop_geometry() %>%
  group_by(gridid) %>%
  summarise(
    length_ltn = sum(length[which(ratrun == "no")]),
    length_ratrun = sum(length[which(ratrun == "yes")]),
    length_calmed = sum(length[which(ratrun == "calmed")]),
    length_main = sum(length[which(ratrun == "main")])
  )

ltn_grid = left_join(grid, ltn_aggregated)
plot(ltn_grid)
mapview(ltn_grid["length_main"])





identical(names(rnet2), names(grid_split))

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
