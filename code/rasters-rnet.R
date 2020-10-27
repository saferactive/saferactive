# Aim: get raster grid cell estimate of cycling levels based on rnet from pct
library(sf)
library(tidyverse)

if(file.exists("rnet_national_sf_27700.Rds")){
  rnet_national_sf_27700 <- readRDS("rnet_national_sf_27700.Rds")
} else {
  rnet_national_sf_27700 <- read_sf("https://github.com/npct/pct-outputs-national/raw/master/commute/lsoa/rnet_all.geojson")
  rnet_national_sf_27700 <- st_transform(rnet_national_sf_27700, 27700)
  saveRDS(rnet_national_sf_27700, "rnet_national_sf_27700.Rds")
}

# rnet_national_sp = readRDS("~/npct/pct-outputs-national/commute/lsoa/rnet_all.Rds")
# system.time({
#   rnet_centroids = rgeos::gCentroid(rnet_national_sp, byid = TRUE)
# })
#
# class(rnet_centroids)
# length(rnet_centroids)
# names(rnet_centroids)
#
# system.time({
#   rnet_national_sf = sf::st_as_sf(rnet_national_sp)
#   rnet_national_sf$length = as.numeric(sf::st_length(rnet_national_sf))
# })
#
# system.time({
#   rnet_national_sf_27700 = sf::st_transform(rnet_national_sf, 27700)
# })
source("code/osm_cleaning_functions.R")

rast_template = raster::raster("rasters/allmode_alltime_all.tif")
names(rast_template)

rnet_split = line_segment_sf(l = rnet_national_sf_27700, segment_length = 50)


# preprocessing step: break-up long segments
rnet_longest_1000 = rnet_national_sf_27700 %>% filter(length > 1000)
rnet_short = rnet_national_sf_27700 %>% filter(length <= 1000)
rast_template_polys = rast_template %>% stars::st_as_stars() %>% sf::st_as_sf(as_points = FALSE)
sf::st_area(raster_rnet_polys$geometry[1])
# 250000 [m^2]
mapview::mapview(raster_rnet_polys$geometry[1])
system.time({
  nrow(rnet_longest_1000)
  rnet_longest_split = line_breakup(rnet_longest_1000, rast_template_polys)
  rnet_longest_split$length = as.numeric(sf::st_length(rnet_longest_split))
  nrow(rnet_longest_split)
  rnet_updated = rbind(rnet_short, rnet_longest_split)
})
# user   system  elapsed
# 2438.324    0.422 2437.605
# 2500 / 60
nrow(rnet_updated) / nrow(rnet_national_sf_27700)
# [1] 1.0056
saveRDS(rnet_updated, "rnet_updated_100m.Rds")

system.time({
  rnet_cents = sf::st_centroid(rnet_updated)
})
names(rnet_cents)
# See https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/888754/amat-user-guidance.pdf
# for 253 days per year estimate
rnet_cents$km_cycled_yr = rnet_cents$length * rnet_cents$bicycle * 253 * 2 / 1000
rnet_cents$kkm_cycled_yr = rnet_cents$length * rnet_cents$bicycle * 253 * 2 / 1000000
sum(rnet_cents$km_cycled_yr) / 4475741485
# [1] 0.2499664 # nearly quarter of the ~4.5 billion bkm cycled in England and Wales according to the NTS: NTS0303
# see code/nts-national-distance-cycled-year.R script for details
saveRDS(rnet_cents, "rnet_cents.Rds")

# Rasterising the results - see rasters.R
system.time({
  raster_rnet_bicycle = raster::rasterize(rnet_cents, rast_template, field = "kkm_cycled_yr", fun = sum, na.rm = TRUE)
})
# 7 s
mapview::mapview(raster_rnet_bicycle)

writeRaster(raster_rnet_bicycle, "raster_rnet_bicycle.tif", overwrite = TRUE)
piggyback::pb_upload("raster_rnet_bicycle.tif")
system("mv -v *.tif rasters")
# 'raster_rnet_bicycle.tif' -> 'rasters/raster_rnet_bicycle.tif'

library(tmap)
tmap_mode("view")
qtm(raster_rnet_bicycle, "bicycle")
# why are so many points empty? https://github.com/saferactive/saferactive/issues/48#issuecomment-713869812
rnet_cents %>% sample_frac(size = 0.01) %>% mapview::mapview()


# Break-up linestrings ----------------------------------------------------

# https://gis.stackexchange.com/questions/181011/fill-the-gaps-using-nearest-neighbors
summary(rnet_cents$length)
sum(rnet_cents$kkm_cycled_yr[rnet_cents$length > 500]) / sum(rnet_cents$kkm_cycled_yr)
# 31% of all cycling activity is on segments more than 500m in length
# u = "https://github.com/npct/pct-outputs-regional-notR/raw/master/commute/lsoa/london/rnet_full.geojson"
# rnet_lnd = sf::read_sf(u)
# # this led to a new function in stplanr: line_breakup:
# # https://github.com/ropensci/stplanr/issues/434
remotes::install_github("ropensci/stplanr")
library(stplanr)

raster_rnet_bicycle = raster::raster("rasters/raster_rnet_bicycle.tif")

# test for London breakup test for London
london = spData::lnd %>% sf::st_transform(27700)
raster_rnet_lnd = raster::crop(raster_rnet_bicycle, london)
raster_rnet_lnd_polys = raster_rnet_lnd %>% stars::st_as_stars() %>% sf::st_as_sf(as_points = FALSE)
plot(raster_rnet_lnd_polys)
rnet_updated = readRDS("rnet_updated.Rds")
rnet_lnd = rnet_national_sf_27700[london, ]
rnet_longest_500 = rnet_lnd %>% filter(length > 500)
nrow(rnet_longest_500)
# [1] 2635
mapview::mapview(rnet_longest_500)
rnet_longest_1000 = rnet_lnd %>% filter(length > 1000)
nrow(rnet_longest_1000)
# [1] 498 1/5th
mapview::mapview(rnet_longest_1000)
rnet_longest_1pc = rnet_longest_1000 %>% sample_frac(0.01)


# test on 1% sample
system.time({
  rnet_longest2 = rnet_longest_1pc
  nrow(rnet_longest2)
  rnet_longest2_split = line_breakup(rnet_longest2, raster_rnet_lnd_polys)
  rnet_longest2_split$length = as.numeric(sf::st_length(rnet_longest2_split))
  nrow(rnet_longest2_split)
  rnet_short = rnet_lnd %>% filter(length <= 1000)
  rnet_updated = rbind(rnet_short, rnet_longest2_split)
})
# user  system elapsed
# 4.179   0.004   4.219
4.5 * 100 / 60 # should take around 10 minutes...


system.time({
  rnet_longest2 = rnet_longest_1000
  nrow(rnet_longest2)
  rnet_longest2_split = line_breakup(rnet_longest2, raster_rnet_lnd_polys)
  rnet_longest2_split$length = as.numeric(sf::st_length(rnet_longest2_split))
  nrow(rnet_longest2_split)
  rnet_short = rnet_lnd %>% filter(length <= 1000)
  rnet_updated = rbind(rnet_short, rnet_longest2_split)
})

nrow(rnet_updated) / nrow(rnet_lnd)
# [1] 1.024677 # 2% more rows associated with longest segments now split-up
sum(rnet_lnd$length)
sum(rnet_updated$length)
sum(rnet_short$length)

rnet_cents_updated = sf::st_centroid(rnet_updated)
names(rnet_cents_updated)
# for 253 days per year estimate
rnet_cents_updated$km_cycled_yr = rnet_cents_updated$length * rnet_cents_updated$bicycle * 253 * 2 / 1000
rnet_cents_updated$kkm_cycled_yr = rnet_cents_updated$length * rnet_cents_updated$bicycle * 253 * 2 / 1000000
sum(rnet_cents_updated$km_cycled_yr) / 4475741485
# [1] 0.2499664 # nearly quarter of the ~4.5 billion bkm cycled in England and Wales according to the NTS: NTS0303
# see code/nts-national-distance-cycled-year.R script for details
saveRDS(rnet_cents_updated, "rnet_cents_updated.Rds")
rast_template = raster::raster("rasters/allmode_alltime_all.tif")
rast_template = raster::raster(london, resolution = 500)
raster_rnet_bicycle = raster::rasterize(rnet_cents_updated, rast_template, field = "kkm_cycled_yr", fun = sum, na.rm = TRUE)
mapview::mapview(raster_rnet_bicycle)

# Fill NAs ----------------------------------------------------------------

# library(raster)
# raster_rnet_bicycle = raster::raster("rasters/raster_rnet_bicycle.tif")
# raster_rnet_lnd = raster::crop(raster_rnet_bicycle, london)
# sum(values(raster_rnet_bicycle), na.rm = TRUE) / 4475741
# # [1] 0.2499664
#
# summary(values(raster_rnet_bicycle))
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
# # 0.0     0.5     2.4     9.9     8.0  2067.8 2724572
# fill.na = function(x, i=5) {
#   if( is.na(x)[i] ) {
#     return(mean(x, na.rm = TRUE))
#   } else {
#     return(x[i])
#   }
# }
# mapview::mapview(raster_rnet_lnd)
# raster_rnet_lnd_filled = focal(raster_rnet_lnd, w = matrix(1, 3, 3), fun = fill.na, pad = TRUE, na.rm = FALSE)
# mapview::mapview(raster_rnet_lnd_filled) # much better
# raster_rnet_filled = focal(raster_rnet_bicycle, w = matrix(1, 3, 3), fun = fill.na, pad = TRUE, na.rm = FALSE)
# mapview::mapview(raster_rnet_filled)
# summary(values(raster_rnet_filled))
# sum(values(raster_rnet_filled), na.rm = TRUE) / 4475741 # has a huge impact on result
#
#
# # Smooth prediction -------------------------------------------------------
#
# library(raster)
# raster_rnet_bicycle = raster::raster("rasters/raster_rnet_bicycle.tif")
# summary(values(raster_rnet_bicycle))
#
# raster_rnet_df = raster::as.data.frame(raster_rnet_bicycle, xy = TRUE)
# head(raster_rnet_df)
# raster_rnet_sf = sf::st_as_sf(raster_rnet_df, coords = c("x", "y"), crs = 27700)
# nrow(raster_rnet_sf) # ~ 3m
# raster_rnet_sf = raster_rnet_sf[!is.na(raster_rnet_sf$raster_rnet_bicycle), ]
# nrow(raster_rnet_sf) # ~ 100k
# plot(raster_rnet_sf, cex = 0.01, breaks = c(0, 10, 100, 500, 1000))
# summary(raster_rnet_sf$raster_rnet_bicycle)
#
#
# # Try spatial interpolation -----------------------------------------------
#
# library(sf)
# library(gstat)
# library(stars)
#
# # test for london
# london = spData::lnd %>% sf::st_transform(27700)
#
# # rnet_lnd = readRDS(url(u))
# rnet_lnd = raster_rnet_sf[london, ]
# rnet_lnd
# grd = st_as_stars(raster_rnet_bicycle)
# # grd = st_bbox(rnet_lnd) %>%
# #   st_as_stars(dx = 500, dy = 500) %>%
# #   st_set_crs(27700) %>%
# #   st_crop(rnet_lnd)
# st_crs(grd) = 27700
# grd_lnd = grd[london, ]
# plot(grd_lnd)
# rnet_lnd$kkm_cycled = rnet_lnd$raster_rnet_bicycle
# v = variogram(kkm_cycled~1, rnet_lnd, cutoff = 20000)
# plot(v)
# vm = fit.variogram(v, vgm(psill = "Sph", model = "Exp"))
# plot(vm, cutoff = 10000)
# rnet_krige = gstat::krige(kkm_cycled~1, rnet_lnd, grd, vm, nmax = 10)
# plot(rnet_lnd$geometry)
# plot(rnet_krige, add = TRUE)
#
# mapview::mapview(rnet_krige)
#
# rnet_krige = gstat::krige(bicycle~1, rnet_lnd, grd, vm)
# mapview::mapview(rnet_krige)

# Next step: estimate smooth surface of million km cycled/500m cell
# see: https://keen-swartz-3146c4.netlify.app/interpolation.html
# rast_template = raster::raster("rasters/allmode_alltime_all.tif")
# summary(raster::values(rast_template))
# raster::values(rast_template) = NA
# grd = stars::st_as_stars(rast_template)
# identical(sf::st_crs(grd), sf::st_crs(rnet_cents))
# sf::st_crs(grd) = sf::st_crs(rnet_cents)
#
# rnet_cents = readRDS("rnet_cents.Rds")
# nrow(rnet_cents) # ~1/2 million
# rnet_cents
#
# remotes::install_cran("gstat")
# library(gstat)
#
# # test for london
# london = spData::lnd %>% sf::st_transform(27700)
# grd_lnd = grd[london, ]
# rnet_lnd = rnet_cents[london, ]
# mapview::mapview(grd_lnd)
# # ggplot() + stars::geom_stars(data = grd_lnd, aes(fill = values, x = x, y = y))
# v = variogram(bicycle~1, rnet_lnd, cutoff = 5000)
# plot(v)
# vm = fit.variogram(v, vgm(psill = "Sph", model = "Exp"))
# plot(v, vm, cutoff = 5000)
#
# rnet_lnd_1pcnt = rnet_lnd %>% sample_frac(0.01)
# system.time({
#   rnet_krige1 = gstat::krige(bicycle~1, rnet_lnd_1pcnt, grd_lnd, vm, nmax = 9)
# })
# # warning:   complete map seems to be NA's -- no selection was made
#
# # takes some time...
# library(stars)
# st_bbox(rnet_lnd) %>%
#   st_as_stars(dx = 500) %>%
#   st_set_crs(27700) %>%
#   st_crop(rnet_lnd) -> grd
#
# system.time({
#   rnet_krige1 = gstat::krige(bicycle~1, rnet_lnd_1pcnt, grd, vm, nmax = 9)
# })
#
#
# rnet_krige1
# plot(rnet_krige1)
# # it works!
#
# # larger example for London
#
# system.time({
#   rnet_krige2 = gstat::krige(bicycle~1, rnet_lnd, grd, vm, nmax = 300)
# })
# # 25 s
# plot(rnet_krige2)
#
#
# system.time({
#   rnet_krige3 = gstat::krige(bicycle~1, rnet_lnd, grd, vm, nmax = 300, maxdist = 1000)
# })
# # 8 s
# plot(rnet_krige3, breaks = c(0, 5, 10, 100, 1000, 5000, 10000)) # looking good but there are many blocks with NA values
#
# #
# system.time({
#   rnet_krige4 = gstat::krige(bicycle~1, rnet_lnd, grd, vm, nmax = 100, maxdist = 1000, na.action = na.pass)
# })
# # 12 s
# plot(london$geometry)
# plot(rnet_krige4, add = TRUE)
#
# system.time({
#   rnet_krige5 = gstat::krige(bicycle~1, rnet_lnd, grd, vm, nmax = 100, maxdist = 1000, na.action = na.pass, beta = 10)
# })
# plot(rnet_krige5)
# rnet_krige4 = gstat::krige(bicycle~1, rnet_lnd_1pcnt, grd, vm, nmax = 100, maxdist = 1000, na.action = na.pass, beta = 10)
# plot(rnet_krige4)
#
# # ask for help...
#
# saveRDS(rnet_lnd, "rnet_lnd.Rds")
# piggyback::pb_upload("rnet_lnd.Rds")
# piggyback::pb_download_url("rnet_lnd.Rds")
#
# saveRDS(rnet_lnd_1pcnt, "rnet_lnd_1pcnt.Rds")
# piggyback::pb_upload("rnet_lnd_1pcnt.Rds")
# piggyback::pb_download_url("rnet_lnd_1pcnt.Rds")
#
# # reprex
# library(sf)
# library(gstat)
# library(stars)
# u = "https://github.com/saferactive/saferactive/releases/download/0.1.1/rnet_lnd_1pcnt.Rds"
# rnet_lnd = readRDS(url(u))
# rnet_lnd
# grd = st_bbox(rnet_lnd) %>%
#   st_as_stars(dx = 500, dy = 500) %>%
#   st_set_crs(27700)
# grd
# v = variogram(bicycle~1, rnet_lnd, cutoff = 10000)
# plot(v)
# vm = fit.variogram(v, vgm(psill = "Sph", model = "Exp", range = 10000, nugget = 1))
# plot(vm, cutoff = 10000)
# rnet_krige = gstat::krige(bicycle~1, rnet_lnd, grd, vm, nmax = 100)
# plot(rnet_lnd$geometry)
# plot(rnet_krige, add = TRUE)
#
# mapview::mapview(rnet_krige)
#
# rnet_krige = gstat::krige(bicycle~1, rnet_lnd, grd, vm)
# mapview::mapview(rnet_krige)

## rnet breakup tests
#
# rnet_longest = rnet_lnd %>% slice(which.max(length))
# mapview::mapview(rnet_longest) # a very long segment
# stplanr::n_vertices(rnet_longest) # 161 vertices - quite a lot
# rnet_broken_up1 = lwgeom::st_split(rnet_longest, raster_rnet_lnd_polys) %>%
#   sf::st_collection_extract(type = "LINESTRING")
# sf::st_cast("LINESTRING")
# nrow(rnet_broken_up1) # 20!
# plot(rnet_broken_up1)
# rnet_broken_up1$length = sf::st_length(rnet_broken_up1)
# plot(rnet_broken_up1["length"])
# mapview::mapview(raster_rnet_lnd) +
#   mapview::mapview(rnet_broken_up1)
# sum(sf::st_length(rnet_broken_up1))
# rnet_longest$length # broadly the same
#
# # this led to a new function in stplanr: line_breakup:
# # https://github.com/ropensci/stplanr/issues/434
# remotes::install_github("ropensci/stplanr")
# rnet_longest2 = rnet_lnd %>%
#   filter(length > 7000)
# nrow(rnet_longest2) # 4
# library(stplanr)
# rnet_longest2_split = line_breakup(rnet_longest2, raster_rnet_lnd_polys)
# rnet_longest2_split$length = as.numeric(sf::st_length(rnet_longest2_split))
# nrow(rnet_longest2_split) # 77 - big increase in n. segments
#
# rnet_short = rnet_lnd %>% filter(length < 7000)
# rnet_updated = rbind(rnet_short, rnet_longest2_split)
# nrow(rnet_updated)
# # [1] 70139
# nrow(rnet_lnd)
# # [1] 70066
#
# sum(rnet_longest2_split$length)
# sum(rnet_longest2$length)
# mapview::mapview(rnet_longest2_split["length"])

