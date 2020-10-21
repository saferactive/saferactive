library(dplyr)
library(lubridate)
library(raster)
library(sf)
library(stars)

piggyback::pb_download_url("acc_with_cas.Rds")
u = "https://github.com/saferactive/saferactive/releases/download/0.1.1/acc_with_cas.Rds"
f = basename(u)

if(file.exists("acc_with_cas.Rds")){
  acc <- readRDS("acc_with_cas.Rds")
} else {
  download.file(u, r)
  acc <- readRDS("acc_with_cas.Rds")
  # uncomment the following to reproduce:
  # Bring in Crash Data
  # cas <- stats19::get_stats19(2010:2019, type = "cas")
  # acc <- stats19::get_stats19(2010:2019, type = "acc", output_format = "sf")
  #
  # cas <- cas[,c("accident_index","casualty_type","age_band_of_casualty","casualty_severity")]
  # acc <- acc[,c("accident_index","datetime","accident_severity","day_of_week",
  #               "junction_detail","junction_control","police_force")]
  #
  #   # Identify Active Travelers and Commuters
  # cas_summary <- group_by(cas, accident_index) %>%
  #   summarise(total_cas = n(),
  #             total_cas_ksi = length(casualty_type[casualty_severity != "Slight"]),
  #             cycle_cas = length(casualty_type[casualty_type == "Cyclist"]),
  #             ped_cas = length(casualty_type[casualty_type == "Pedestrian"]),
  #             cycle_cas_ksi = length(casualty_type[casualty_type == "Cyclist" & casualty_severity != "Slight"]),
  #             ped_cas_ksi = length(casualty_type[casualty_type == "Pedestrian" & casualty_severity != "Slight"]),
  #   )
  #
  #
  # acc <- left_join(acc, cas_summary, by = "accident_index")
  # acc$year <- year(acc$datetime)
  # saveRDS(acc, "acc_with_cas.Rds")
}

piggyback::pb_upload("acc_with_cas.Rds")
piggyback::pb_download_url("acc_with_cas.Rds")
# [1] "https://github.com/saferactive/saferactive/releases/download/0.1.1/acc_with_cas.Rds"


summary(acc)

# accident_index        datetime                   accident_severity  day_of_week        junction_detail    junction_control
# Length:1383135     Min.   :2010-01-01 00:01:00   Length:1383135     Length:1383135     Length:1383135     Length:1383135
# Class :character   1st Qu.:2012-04-16 09:02:30   Class :character   Class :character   Class :character   Class :character
# Mode  :character   Median :2014-09-15 14:00:00   Mode  :character   Mode  :character   Mode  :character   Mode  :character
# Mean   :2014-10-14 02:31:12
# 3rd Qu.:2017-03-10 14:44:30
# Max.   :2019-12-31 23:53:00
# NA's   :132
#  police_force                geometry         total_cas      total_cas_ksi       cycle_cas          ped_cas        cycle_cas_ksi
#  Length:1383135     POINT        :1383135   Min.   : 1.000   Min.   : 0.0000   Min.   : 0.0000   Min.   : 0.0000   Min.   :0.0000
#  Class :character   epsg:27700   :      0   1st Qu.: 1.000   1st Qu.: 0.0000   1st Qu.: 0.0000   1st Qu.: 0.0000   1st Qu.:0.0000
#  Mode  :character   +proj=tmer...:      0   Median : 1.000   Median : 0.0000   Median : 0.0000   Median : 0.0000   Median :0.0000
#                                             Mean   : 1.329   Mean   : 0.1834   Mean   : 0.1347   Mean   : 0.1747   Mean   :0.0248
#                                             3rd Qu.: 1.000   3rd Qu.: 0.0000   3rd Qu.: 0.0000   3rd Qu.: 0.0000   3rd Qu.:0.0000
#                                             Max.   :93.000   Max.   :20.0000   Max.   :10.0000   Max.   :22.0000   Max.   :5.0000
#
#   ped_cas_ksi            year
#  Min.   : 0.00000   Min.   :2010
#  1st Qu.: 0.00000   1st Qu.:2012
#  Median : 0.00000   Median :2014
#  Mean   : 0.04187   Mean   :2014
#  3rd Qu.: 0.00000   3rd Qu.:2017
#  Max.   :11.00000   Max.   :2019
#                     NA's   :132


# Filter to London
# acc <- acc[acc$police_force %in% c("Metropolitan Police","City of London"), ]

acc <- acc[!is.na(acc$year),]
acc <- acc[!st_is_empty(acc),]


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


  message(Sys.time()," Year: ",i, " crashes: ",nrow(acc_sub))
  rast_sub_all <- rasterize(acc_sub, rast_template, field = "cycle_cas", fun = "sum")
  rast_sub_ksi <- rasterize(acc_sub, rast_template, field = "cycle_cas_ksi", fun = "sum")
  rast_cycle_commute_all <- addLayer(rast_cycle_commute_all, rast_sub_all)
  rast_cycle_commute_ksi <- addLayer(rast_cycle_commute_ksi, rast_sub_ksi)
}

writeRaster(rast_cycle_commute_all,"rasters/cycle_commute_all.tif", overwrite=TRUE, datatype = "INT2U")
writeRaster(rast_cycle_commute_ksi,"rasters/cycle_commute_ksi.tif", overwrite=TRUE, datatype = "INT2U")

rast_cycle_alltime_all <- brick(rast_template)
rast_cycle_alltime_ksi <- brick(rast_template)

for(i in 2010:2019){
  acc_sub <- acc[acc$year == i,]
  acc_sub <- acc_sub[acc_sub$ped_cas > 0, ]

  message(Sys.time()," Year: ",i, " crashes: ",nrow(acc_sub))
  rast_sub_all <- rasterize(acc_sub, rast_template, field = "cycle_cas", fun = "sum")
  rast_sub_ksi <- rasterize(acc_sub, rast_template, field = "cycle_cas_ksi", fun = "sum")
  rast_cycle_alltime_all <- addLayer(rast_cycle_alltime_all, rast_sub_all)
  rast_cycle_alltime_ksi <- addLayer(rast_cycle_alltime_ksi, rast_sub_ksi)
}

writeRaster(rast_cycle_alltime_all,"rasters/cycle_alltime_all.tif", overwrite=TRUE, datatype = "INT2U")
writeRaster(rast_cycle_alltime_ksi,"rasters/cycle_alltime_ksi.tif", overwrite=TRUE, datatype = "INT2U")

# Walking -----------------------------------------------------------------

rast_walk_commute_all <- brick(rast_template)
rast_walk_commute_ksi <- brick(rast_template)

for(i in 2010:2019){
  acc_sub <- acc[acc$year == i,]
  acc_sub <- acc_sub[acc_sub$ped_cas > 0, ]
  acc_sub <- acc_sub[!acc_sub$day_of_week %in% c("Saturday","Sunday"), ]
  acc_sub <- acc_sub[(hour(acc_sub$datetime) > 7 & hour(acc_sub$datetime) < 10) |
                       (hour(acc_sub$datetime) > 16 & hour(acc_sub$datetime) < 19), ]

  message(Sys.time()," Year: ",i, " crashes: ",nrow(acc_sub))
  rast_sub_all <- rasterize(acc_sub, rast_template, field = "ped_cas", fun = "sum")
  rast_sub_ksi <- rasterize(acc_sub, rast_template, field = "ped_cas_ksi", fun = "sum")
  rast_walk_commute_all <- addLayer(rast_walk_commute_all, rast_sub_all)
  rast_walk_commute_ksi <- addLayer(rast_walk_commute_ksi, rast_sub_ksi)
}

writeRaster(rast_walk_commute_all,"rasters/walk_commute_all.tif", overwrite=TRUE, datatype = "INT2U")
writeRaster(rast_walk_commute_ksi,"rasters/walk_commute_ksi.tif", overwrite=TRUE, datatype = "INT2U")


rast_walk_alltime_all <- brick(rast_template)
rast_walk_alltime_ksi <- brick(rast_template)


for(i in 2010:2019){
  acc_sub <- acc[acc$year == i,]
  acc_sub <- acc_sub[acc_sub$ped_cas > 0, ]

  message(Sys.time()," Year: ",i, " crashes: ",nrow(acc_sub))
  rast_sub_all <- rasterize(acc_sub, rast_template, field = "ped_cas", fun = "sum")
  rast_sub_ksi <- rasterize(acc_sub, rast_template, field = "ped_cas_ksi", fun = "sum")
  rast_walk_alltime_all <- addLayer(rast_walk_alltime_all, rast_sub_all)
  rast_walk_alltime_ksi <- addLayer(rast_walk_alltime_ksi, rast_sub_ksi)
}

writeRaster(rast_walk_alltime_all,"rasters/walk_alltime_all.tif", overwrite=TRUE, datatype = "INT2U")
writeRaster(rast_walk_alltime_ksi,"rasters/walk_alltime_ksi.tif", overwrite=TRUE, datatype = "INT2U")


# All Mode ----------------------------------------------------------------

rast_allmode_commute_all <- brick(rast_template)
rast_allmode_commute_ksi <- brick(rast_template)

for(i in 2010:2019){
  acc_sub <- acc[acc$year == i,]
  acc_sub <- acc_sub[acc_sub$total_cas > 0, ]
  acc_sub <- acc_sub[!acc_sub$day_of_week %in% c("Saturday","Sunday"), ]
  acc_sub <- acc_sub[(hour(acc_sub$datetime) > 7 & hour(acc_sub$datetime) < 10) |
                       (hour(acc_sub$datetime) > 16 & hour(acc_sub$datetime) < 19), ]

  message(Sys.time()," Year: ",i, " crashes: ",nrow(acc_sub))
  rast_sub_all <- rasterize(acc_sub, rast_template, field = "total_cas", fun = "sum")
  rast_sub_ksi <- rasterize(acc_sub, rast_template, field = "total_cas_ksi", fun = "sum")
  rast_allmode_commute_all <- addLayer(rast_allmode_commute_all, rast_sub_all)
  rast_allmode_commute_ksi <- addLayer(rast_allmode_commute_ksi, rast_sub_ksi)
}

writeRaster(rast_allmode_commute_all,"rasters/allmode_commute_all.tif", overwrite=TRUE, datatype = "INT2U")
writeRaster(rast_allmode_commute_ksi,"rasters/allmode_commute_ksi.tif", overwrite=TRUE, datatype = "INT2U")


rast_allmode_alltime_all <- brick(rast_template)
rast_allmode_alltime_ksi <- brick(rast_template)


for(i in 2010:2019){
  acc_sub <- acc[acc$year == i,]
  acc_sub <- acc_sub[acc_sub$total_cas > 0, ]

  message(Sys.time()," Year: ",i, " crashes: ",nrow(acc_sub))
  rast_sub_all <- rasterize(acc_sub, rast_template, field = "total_cas", fun = "sum")
  rast_sub_ksi <- rasterize(acc_sub, rast_template, field = "total_cas_ksi", fun = "sum")
  rast_allmode_alltime_all <- addLayer(rast_allmode_alltime_all, rast_sub_all)
  rast_allmode_alltime_ksi <- addLayer(rast_allmode_alltime_ksi, rast_sub_ksi)
}

writeRaster(rast_allmode_alltime_all,"rasters/allmode_alltime_all.tif", overwrite=TRUE, datatype = "INT2U")
writeRaster(rast_allmode_alltime_ksi,"rasters/allmode_alltime_ksi.tif", overwrite=TRUE, datatype = "INT2U")

# get raster datasets:
# system("echo *.tif >> .gitignore")
# dir.create("rasters")
# pbdls = piggyback::pb_list()
# pbras = pbdls %>% filter(stringr::str_detect(string = file_name, pattern = ".tif"))
# pbras[1, ] %>% pull(file_name) %>% piggyback::pb_download_url()
# pbras$url = paste0("https://github.com/saferactive/saferactive/releases/download/0.1.1/", pbras$file_name)
# readr::write_csv(pbras, "small-output-datasets/raster-filenames.csv")
pbras = readr::read_csv("small-output-datasets/raster-filenames.csv")
for(i in seq(nrow(pbras))) {
  download.file(url = pbras$url[i], paste0("rasters/", pbras$file_name[i]))
}

r1 = raster::raster("rasters/allmode_alltime_all.tif")
r1 = raster::raster("rasters/cycle_commute_ksi.tif")

r1
plot(r1)
mapview::mapview(r1)
raster::plotRGB(r1)

b1 = raster::brick("rasters/cycle_commute_ksi.tif")
b1
plot(b1)
