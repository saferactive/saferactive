library(sf)
library(geoplumber)
# read in the Rds
#' ################
ow = setwd("ignored")
if(!file.exists("agg_0518.Rds")) stop("get the RDS for 79 to 18")
# should be looking at agg7918.Rds
agg = readRDS("agg_0518.Rds")

# get geography
#' ################
geo = sf::st_read("~/Downloads/ltlas.geojson")
agg_geo = agg[agg$local_authority_district %in% geo$lad19nm,]
m = match(agg_geo$local_authority_district, geo$lad19nm)
agg_geo = sf::st_as_sf(agg_geo, st_geometry(geo)[m])

# vis!
#' ################
setwd("~/code/geoplumber/geoplumber/")
gp_is_wd_geoplumber()
r = gp_explore(agg_geo)
# might not appear straight in viewer
# reload will work, this is an issue for geoplumber.

#' take a screen shot? not.
#' ################
library(webdriver)
pjs <- run_phantomjs()
pjs$process
ses <- Session$new(port = pjs$port)
ses$go("http://localhost:8000/explore")
grep("main.", ses$getSource()) == 1
# for some reason webdriver does not execute js to a screenshot
# it might be me not knowing enough about the R package.
# ses$takeScreenshot(file = "gp.png")


# shutdown gp
#' ################
r$kill()

# shutdown webdriver
#' ################
pjs$process$kill()
r$kill()

# exit
#' ################
setwd(ow)
