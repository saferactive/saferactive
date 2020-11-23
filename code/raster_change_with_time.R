# Change over time
library(raster)


rast_brick <- brick("rasters/cycle_alltime_all.tif")

rast_old <- rast_brick[[1]]
rast_new <- rast_brick[[10]]

rast_diff <- rast_new - rast_old
summary(rast_diff)
