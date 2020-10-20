library(dplyr)
library(lubridate)
library(raster)
library(sf)
library(stars)

if(file.exists("acc_with_cas.Rds")){
  acc <- readRDS("acc_with_cas.Rds")
} else {
  # Bring in Crash Data
  cas <- stats19::get_stats19(2010:2019, type = "cas")
  acc <- stats19::get_stats19(2010:2019, type = "acc", output_format = "sf")

  cas <- cas[,c("accident_index","casualty_type","age_band_of_casualty","casualty_severity")]
  acc <- acc[,c("accident_index","datetime","accident_severity","day_of_week",
                "junction_detail","junction_control","police_force")]

    # Identify Active Travelers and Commuters
  cas_summary <- group_by(cas, accident_index) %>%
    summarise(total_cas = n(),
              total_cas_ksi = length(casualty_type[casualty_severity != "Slight"]),
              cycle_cas = length(casualty_type[casualty_type == "Cyclist"]),
              ped_cas = length(casualty_type[casualty_type == "Pedestrian"]),
              cycle_cas_ksi = length(casualty_type[casualty_type == "Cyclist" & casualty_severity != "Slight"]),
              ped_cas_ksi = length(casualty_type[casualty_type == "Pedestrian" & casualty_severity != "Slight"]),
    )


  acc <- left_join(acc, cas_summary, by = "accident_index")
  acc$year <- year(acc$datetime)
  saveRDS(acc, "acc_with_cas.Rds")
}

# Filter to London
# acc <- acc[acc$police_force %in% c("Metropolitan Police","City of London"), ]

#TODO: NAs in the year field due to NA datetime

# Make Raster Bricks ------------------------------------------------------

# Walk/Cycle/Both/All
# Commuter/All Time
# KSI / Slight

rast_template <- raster(acc, resolution = c(500,500))


rast_cycle_commute_all <- brick(rast_template)
rast_cycle_commute_ksi <- brick(rast_template)


# Cycling -----------------------------------------------------------------


for(i in 2010:2019){
  acc_sub <- acc[acc$year == i,]
  acc_sub <- acc_sub[acc_sub$cycle_cas > 0, ]
  acc_sub <- acc_sub[!acc_sub$day_of_week %in% c("Saturday","Sunday"), ]
  acc_sub <- acc_sub[(hour(acc_sub$datetime) > 7 & hour(acc_sub$datetime) < 10) |
                                           (hour(acc_sub$datetime) > 16 & hour(acc_sub$datetime) < 19), ]
  acc_sub <- acc_sub[!st_is_empty(acc_sub),]

  message(Sys.time()," Year: ",i, " crashes: ",nrow(acc_sub))
  rast_sub_all <- rasterize(acc_sub, rast_template, field = "cycle_cas", fun = "sum")
  rast_sub_ksi <- rasterize(acc_sub, rast_template, field = "cycle_cas_ksi", fun = "sum")
  rast_cycle_commute_all <- addLayer(rast_cycle_commute_all, rast_sub_all)
  rast_cycle_commute_ksi <- addLayer(rast_cycle_commute_ksi, rast_sub_ksi)
}

writeRaster(rast_cycle_commute_all,"rasters/cycle_commute_all.tif", overwrite=TRUE)
writeRaster(rast_cycle_commute_ksi,"rasters/cycle_commute_ksi.tif", overwrite=TRUE)

rast_cycle_alltime_all <- brick(rast_template)
rast_cycle_alltime_ksi <- brick(rast_template)

for(i in 2010:2019){
  acc_sub <- acc[acc$year == i,]
  acc_sub <- acc_sub[acc_sub$ped_cas > 0, ]
  acc_sub <- acc_sub[!st_is_empty(acc_sub),]

  message(Sys.time()," Year: ",i, " crashes: ",nrow(acc_sub))
  rast_sub_all <- rasterize(acc_sub, rast_template, field = "cycle_cas", fun = "sum")
  rast_sub_ksi <- rasterize(acc_sub, rast_template, field = "cycle_cas_ksi", fun = "sum")
  rast_cycle_alltime_all <- addLayer(rast_cycle_alltime_all, rast_sub_all)
  rast_cycle_alltime_ksi <- addLayer(rast_cycle_alltime_ksi, rast_sub_ksi)
}

writeRaster(rast_cycle_alltime_all,"rasters/cycle_alltime_all.tif", overwrite=TRUE)
writeRaster(rast_cycle_alltime_ksi,"rasters/cycle_alltime_ksi.tif", overwrite=TRUE)

# Walking -----------------------------------------------------------------

rast_walk_commute_all <- brick(rast_template)
rast_walk_commute_ksi <- brick(rast_template)

for(i in 2010:2019){
  acc_sub <- acc[acc$year == i,]
  acc_sub <- acc_sub[acc_sub$ped_cas > 0, ]
  acc_sub <- acc_sub[!acc_sub$day_of_week %in% c("Saturday","Sunday"), ]
  acc_sub <- acc_sub[(hour(acc_sub$datetime) > 7 & hour(acc_sub$datetime) < 10) |
                       (hour(acc_sub$datetime) > 16 & hour(acc_sub$datetime) < 19), ]
  acc_sub <- acc_sub[!st_is_empty(acc_sub),]

  message(Sys.time()," Year: ",i, " crashes: ",nrow(acc_sub))
  rast_sub_all <- rasterize(acc_sub, rast_template, field = "ped_cas", fun = "sum")
  rast_sub_ksi <- rasterize(acc_sub, rast_template, field = "ped_cas_ksi", fun = "sum")
  rast_walk_commute_all <- addLayer(rast_walk_commute_all, rast_sub_all)
  rast_walk_commute_ksi <- addLayer(rast_walk_commute_ksi, rast_sub_ksi)
}

writeRaster(rast_walk_commute_all,"rasters/walk_commute_all.tif", overwrite=TRUE)
writeRaster(rast_walk_commute_ksi,"rasters/walk_commute_ksi.tif", overwrite=TRUE)


rast_walk_alltime_all <- brick(rast_template)
rast_walk_alltime_ksi <- brick(rast_template)


for(i in 2010:2019){
  acc_sub <- acc[acc$year == i,]
  acc_sub <- acc_sub[acc_sub$ped_cas > 0, ]
  acc_sub <- acc_sub[!st_is_empty(acc_sub),]

  message(Sys.time()," Year: ",i, " crashes: ",nrow(acc_sub))
  rast_sub_all <- rasterize(acc_sub, rast_template, field = "ped_cas", fun = "sum")
  rast_sub_ksi <- rasterize(acc_sub, rast_template, field = "ped_cas_ksi", fun = "sum")
  rast_walk_alltime_all <- addLayer(rast_walk_alltime_all, rast_sub_all)
  rast_walk_alltime_ksi <- addLayer(rast_walk_alltime_ksi, rast_sub_ksi)
}

writeRaster(rast_walk_alltime_all,"rasters/walk_alltime_all.tif", overwrite=TRUE)
writeRaster(rast_walk_alltime_ksi,"rasters/walk_alltime_ksi.tif", overwrite=TRUE)


# All Mode ----------------------------------------------------------------

rast_allmode_commute_all <- brick(rast_template)
rast_allmode_commute_ksi <- brick(rast_template)

for(i in 2010:2019){
  acc_sub <- acc[acc$year == i,]
  acc_sub <- acc_sub[acc_sub$total_cas > 0, ]
  acc_sub <- acc_sub[!acc_sub$day_of_week %in% c("Saturday","Sunday"), ]
  acc_sub <- acc_sub[(hour(acc_sub$datetime) > 7 & hour(acc_sub$datetime) < 10) |
                       (hour(acc_sub$datetime) > 16 & hour(acc_sub$datetime) < 19), ]
  acc_sub <- acc_sub[!st_is_empty(acc_sub),]

  message(Sys.time()," Year: ",i, " crashes: ",nrow(acc_sub))
  rast_sub_all <- rasterize(acc_sub, rast_template, field = "total_cas", fun = "sum")
  rast_sub_ksi <- rasterize(acc_sub, rast_template, field = "total_cas_ksi", fun = "sum")
  rast_allmode_commute_all <- addLayer(rast_allmode_commute_all, rast_sub_all)
  rast_allmode_commute_ksi <- addLayer(rast_allmode_commute_ksi, rast_sub_ksi)
}

writeRaster(rast_allmode_commute_all,"rasters/allmode_commute_all.tif", overwrite=TRUE)
writeRaster(rast_allmode_commute_ksi,"rasters/allmode_commute_ksi.tif", overwrite=TRUE)


rast_allmode_alltime_all <- brick(rast_template)
rast_allmode_alltime_ksi <- brick(rast_template)


for(i in 2010:2019){
  acc_sub <- acc[acc$year == i,]
  acc_sub <- acc_sub[acc_sub$total_cas > 0, ]
  acc_sub <- acc_sub[!st_is_empty(acc_sub),]

  message(Sys.time()," Year: ",i, " crashes: ",nrow(acc_sub))
  rast_sub_all <- rasterize(acc_sub, rast_template, field = "total_cas", fun = "sum")
  rast_sub_ksi <- rasterize(acc_sub, rast_template, field = "total_cas_ksi", fun = "sum")
  rast_allmode_alltime_all <- addLayer(rast_allmode_alltime_all, rast_sub_all)
  rast_allmode_alltime_ksi <- addLayer(rast_allmode_alltime_ksi, rast_sub_ksi)
}

writeRaster(rast_allmode_alltime_all,"rasters/allmode_alltime_all.tif", overwrite=TRUE)
writeRaster(rast_allmode_alltime_ksi,"rasters/allmode_alltime_ksi.tif", overwrite=TRUE)
