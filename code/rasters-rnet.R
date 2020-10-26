# Aim: get raster grid cell estimate of cycling levels based on rnet from pct
library(sf)
library(tidyverse)

rnet_national_sp = readRDS("~/npct/pct-outputs-national/commute/lsoa/rnet_all.Rds")
system.time({
  rnet_centroids = rgeos::gCentroid(rnet_national_sp, byid = TRUE)
})

class(rnet_centroids)
length(rnet_centroids)
names(rnet_centroids)

system.time({
  rnet_national_sf = sf::st_as_sf(rnet_national_sp)
  rnet_national_sf$length = as.numeric(sf::st_length(rnet_national_sf))
})

system.time({
  rnet_national_sf_27700 = sf::st_transform(rnet_national_sf, 27700)
})

system.time({
  rnet_cents = sf::st_centroid(rnet_national_sf_27700)
})
# around a minute:
# user  system elapsed
# 41.707  30.251  72.725
names(rnet_cents)
# See https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/888754/amat-user-guidance.pdf
# for 253 days per year estimate
rnet_cents$km_cycled_yr = rnet_cents$length * rnet_cents$bicycle * 253 * 2 / 1000
sum(rnet_cents$km_cycled_yr) / 4475741485
# [1] 0.2499664 # nearly quarter of the ~4.5 billion bkm cycled in England and Wales according to the NTS: NTS0303
# see code/nts-national-distance-cycled-year.R script for details
saveRDS(rnet_cents, "rnet_cents.Rds")

# Rasterising the results - see rasters.R
rast_template = raster::raster("rasters/allmode_alltime_all.tif")
system.time({
  raster_rnet_bicycle = raster::rasterize(rnet_cents, rast_template, field = "km_cycled_yr", fun = sum, na.rm = TRUE)
})
mapview::mapview(raster_rnet_bicycle)

writeRaster(raster_rnet_bicycle, "raster_rnet_bicycle.tif", overwrite=TRUE, datatype = "INT2U")
piggyback::pb_upload("raster_rnet_bicycle.tif")
system("cp -v *.tif rasters")
# 'raster_rnet_bicycle.tif' -> 'rasters/raster_rnet_bicycle.tif'

library(tmap)
tmap_mode("view")
qtm(raster_rnet_bicycle, "bicycle")
# why are so many points empty? https://github.com/saferactive/saferactive/issues/48#issuecomment-713869812
rnet_cents %>% sample_frac(size = 0.01) %>% mapview::mapview()

# Next step: estimate smooth surface of million km cycled/500m cell
# see: https://keen-swartz-3146c4.netlify.app/interpolation.html
rast_template = raster::raster("rasters/allmode_alltime_all.tif")
summary(raster::values(rast_template))
raster::values(rast_template) = NA
grd = stars::st_as_stars(rast_template)
identical(sf::st_crs(grd), sf::st_crs(rnet_cents))
sf::st_crs(grd) = sf::st_crs(rnet_cents)

rnet_cents = readRDS("rnet_cents.Rds")
nrow(rnet_cents) # ~1/2 million
rnet_cents


remotes::install_cran("gstat")
library(gstat)

# test for london
london = spData::lnd

rnet_krige1 = gstat::krige(formula = bicycle~1, rnet_cents, grd)
