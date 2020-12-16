# Aim: estimate risk levels at the 500m grid cell level
library(raster)

cycle_commute_ksi = raster::raster("rasters/cycle_commute_ksi.tif")
cycle_commute_ksi_agg = raster::calc(cycle_commute_ksi, fun = mean)

cycle_commute_all = raster::raster("rasters/cycle_commute_all.tif")
cycle_commute_all_agg = raster::calc(cycle_commute_all, fun = mean)

summary(cycle_commute_all_agg)
summary(cycle_commute_ksi_agg)
plot(cycle_commute_all_agg)
plot(cycle_commute_ksi_agg)
mapview::mapview(cycle_commute_all_agg)

u = "https://github.com/saferactive/saferactive/releases/download/0.1.1/raster_rnet_bicycle.tif"
f = file.path("rasters", basename(u))
if(!file.exists(f)) {
  dir.create("rasters")
  download.file(u, f)
}
raster_rnet_bicycle = raster::raster(f)
mean(values(raster_rnet_bicycle), na.rm = T)
sum(values(raster_rnet_bicycle), na.rm = T)
# 1118513  / 4475741 # units: 1000km cycled:
# https://github.com/saferactive/saferactive/blob/15c6dd151adea1a9727004a9710f82b4f5b07d80/code/rasters-rnet.R
raster_rnet_bicycle[raster_rnet_bicycle < 1] = NA
raster_i_bkm = cycle_commute_all_agg / raster_rnet_bicycle * 1e6
raster_i_bkm[is.nan(raster_i_bkm)] = 0
raster_i_bkm[is.infinite(raster_i_bkm)] = 0

raster_ksi_bkm = cycle_commute_ksi_agg / raster_rnet_bicycle * 1e6
raster_ksi_bkm[is.na(raster_ksi_bkm)] = 0
raster_ksi_bkm[is.infinite(raster_ksi_bkm)] = 0

summary(values(raster_i_bkm))
summary(values(raster_ksi_bkm))

mean(values(raster_ksi_bkm), na.rm = T)

london = spData::lnd %>% sf::st_transform(27700)
raster_ksi_bkm_london = raster::crop(raster_ksi_bkm, london)
raster_ksi_bkm_london_2km = aggregate(raster_ksi_bkm_london, 4, fun = mean)
raster_ksi_bkm_london_4km = aggregate(raster_ksi_bkm_london, 8, fun = mean)
mapview::mapview(raster_ksi_bkm_london)
mapview::mapview(raster_ksi_bkm_london_2km)
mapview::mapview(raster_ksi_bkm_london_4km)


raster_i_bkm_london = raster::crop(raster_i_bkm, london)
raster_i_bkm_london_2km = aggregate(raster_i_bkm_london, 4, fun = mean)
raster_i_bkm_london_4km = aggregate(raster_i_bkm_london, 8, fun = mean)
mapview::mapview(raster_i_bkm_london)
mapview::mapview(raster_i_bkm_london_2km)
mapview::mapview(raster_i_bkm_london_4km)

tmap_mode("view")
brks = c(0, 10, 100, 500, 1000, 5000, 10000)
t1 = tm_shape(raster_i_bkm_london) + tm_raster(palette = "viridis", breaks = brks)

raster_ksi_bkm_lnd =
