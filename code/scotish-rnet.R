# Scottish RNET

remotes::install_github("ropensci/opentripplanner")
remotes::install_github("ITSleeds/UK2GTFS")

library(opentripplanner)
library(sf)
library(UK2GTFS)

path_data = file.path(getwd(),"data_otp")
path_otp = otp_dl_jar(path_data, cache = TRUE)

#gtfs <- atoc2gtfs("D:/OneDrive - University of Leeds/Data/UK2GTFS/ATOC/timetable/2020-11-26/ttis830.zip", ncores = 30)
# gtfs2 <- atoc2gtfs("D:/OneDrive - University of Leeds/Data/UK2GTFS/ATOC/timetable/2020-11-26/ttis830.zip", ncores = 30, locations = "file")
#
# stops2 <- gtfs2$stops
# stops2 <- stops2[!stops2$stop_id %in% stops$stop_id, ]
#
# gtfs_validate_internal(gtfs)
# stops <- gtfs$stops
# stop_times <- gtfs$stop_times
# summary(is.na(stops))
# stops[stops$stop_id == "KINTORE",]
# stop_times[stop_times$stop_id == "KINTORE",]
#
# KINTORE = data.frame(stop_id = "KINTORE",
#                      stop_code = NA,
#                      stop_name = "Kintore railway station",
#                      stop_lat = 57.243611,
#                      stop_lon = -2.350278)
#
# stops = rbind(stops, KINTORE)
# gtfs$stops <- stops
#
# bounds <- readRDS("data/counties_uas_gb_2019_ubc.Rds")
# bounds <- bounds[substr(bounds$ctyua19cd,1,1) == "S",]
# bounds <- st_transform(bounds, 4326)
#
# gtfs_scotland <- gtfs_clip(gtfs, bounds)
# gtfs_write(gtfs_scotland, file.path(path_data,"graphs/default"), name = "gtfs_scotland")

#gtfs_write(gtfs, file.path(path_data,"graphs/default"))
# config <- otp_make_config("router")
# config$routingDefaults$driveOnRight <- FALSE
# config$timeouts <- c(20,10,5,2)
# otp_write_config(config, path_data)

log1 = otp_build_graph(path_otp, path_data, memory = 60000)
log2 = otp_setup(path_otp, path_data, memory = 60000, wait = FALSE)


text <- paste0("java -Xmx", 60000, 'M')
text <- paste0(text, ' -d64 ')
text <- paste0(text, '-jar "',
               path_otp,
               '" --build "',
               path_data,
               "/graphs/",
               "default",
               '"')
system(text)

# Setup request
text <- paste0(
  "java -Xmx", 60000, 'M')

if (TRUE) {
  text <- paste0(text, ' -d64 ')
}

text <- paste0(text, '-jar "',
               path_otp,
               '" --router ', "default",
               ' --graphs "', path_data, '/graphs"',
               " --server --port ", 8080,
               " --securePort ", 8082
)

message(text)

set_up <- try(system(text, intern = FALSE, wait = FALSE))
# system(paste0(
#   "java -d64 -Xmx", 60000, 'M -jar "',
#   path_otp,
#   '" --router ', "default",
#   ' --graphs "', path_data, '/graphs"',
#   " --server --port ", 8080,
#   " --securePort ", 8082
# ))
#
# system('java -version')

# files = list.files("data/terr50_gagg_gb", recursive = TRUE)
#
# dir.create("dem")
#
# for(i in 1:length(files)){
#   dir.create("temp")
#   unzip(file.path(getwd(),"data/terr50_gagg_gb",files[i]),
#         exdir = file.path(getwd(),"temp"))
#   fl <- list.files("temp", pattern = ".asc", full.names = TRUE)
#   fl <- fl[1]
#   file.copy(fl,file.path("dem",strsplit(fl,"/")[[1]][2]))
#   unlink("temp", recursive = TRUE)
# }
#

od <- readRDS("data/scotland_od.Rds")
coords <- stplanr::line2df(od)
od <- st_drop_geometry(od)
geom_from <- st_as_sf(coords[,c("fx","fy")], coords = c("fx","fy"), crs = 4326)
geom_to <- st_as_sf(coords[,c("tx","ty")], coords = c("tx","ty"), crs = 4326)

otpcon <- otp_connect()

# routes <- otp_plan(otpcon,
#                           fromPlace = geom_from,
#                           toPlace = geom_to,
#                           fromID = od$geo_code1,
#                           toID = od$geo_code2,
#                           mode = "BICYCLE",
#                           get_elevation = TRUE,
#                           full_elevation = FALSE,
#                           ncores = 35,
#                           distance_balance = FALSE)
# saveRDS(routes,"data/scotland_routes.Rds")

routes_list <- list()

n <- split(1:nrow(od), cut(1:nrow(od), 5))

for(i in 1:5){
  vals <- n[[i]]
  routes_sub <- try(otp_plan(otpcon,
                             fromPlace = geom_from[vals,],
                             toPlace = geom_to[vals,],
                             fromID = od$geo_code1[vals],
                             toID = od$geo_code2[vals],
                             mode = "BICYCLE",
                             get_elevation = TRUE,
                             full_elevation = FALSE,
                             ncores = 35,
                             distance_balance = TRUE))
  routes_list[[i]] <- routes_sub
}

routes <- dplyr::bind_rows(routes_list)
nrow(routes) - nrow(od)

saveRDS(routes,"data/scotland_routes.Rds")
