# Get and analyse historic OSM data - starter for 10

# install up-to-date osmextract
remotes::install_github("itsleeds/osmextract")
library(osmextract)
library(tidyverse)
library(sf)
file.copy("~/hd/data/osm/internal/greater-london-160101-internal.osm.pbf", ".")
# fails:
# lnd_2016 = osmextract::oe_read(file_path = "~/hd/data/osm/internal/greater-london-160101-internal.osm.pbf", layer = "lines")
# lnd_2016 = osmextract::oe_read(file_path = "greater-london-160101-internal.osm.pbf", layer = "lines")
# ?oe_read
extra_tags = c("maxspeed", "bicycle", "traffic_calming", "oneway", "lanes", "sidewalk", "ref", "lit", "foot", "motor_vehicle")
lnd_2016 = osmextract::oe_read(file_path = "greater-london-160101-internal.osm.pbf", layer = "lines", extra_tags = extra_tags)
lnd_2016$length = sf::st_length(lnd_2016)
lnd = spData::lnd
lnd$NAME
mapview::mapview(lnd)
wf_redbridge = lnd %>%
  filter(stringr::str_detect(string = NAME, pattern = "Wal|Red"))

wf_2016 = lnd_2016[wf_redbridge, , op = st_within]
dim(wf_2016)
# [1] 12698 20

wf_2016 %>% sample_n(1000) %>% mapview::mapview()
table(wf_2016$traffic_calming)

wf_2016j = wf_2016 %>%
  st_join(x = ., y = wf_redbridge["NAME"])
dim(wf_2016j) # bingo
wf_2016s = wf_2016j %>%
  sf::st_drop_geometry() %>%
  group_by(NAME) %>%
  count(highway, wt = length)
wf_2016s$year = 2016

# do for 2018 data
extra_tags = c("maxspeed", "bicycle", "traffic_calming", "oneway", "lanes", "sidewalk", "ref", "lit", "foot", "motor_vehicle")
lnd_2018 = osmextract::oe_read(file_path = "/home/bananafan/Downloads/greater-london-180101.osm.pbf", layer = "lines", extra_tags = extra_tags)
lnd_2018$length = sf::st_length(lnd_2018)

wf_2018 = lnd_2018[wf_redbridge, , op = st_within]
dim(wf_2018)
# [1] 17114    21

wf_2018 %>% sample_n(1000) %>% mapview::mapview()
table(wf_2018$traffic_calming)
# cushion 25

wf_2018j = wf_2018 %>%
  st_join(x = ., y = wf_redbridge["NAME"])
dim(wf_2018j) # bingo columsn = 22
wf_2018s = wf_2018j %>%
  sf::st_drop_geometry() %>%
  group_by(NAME) %>%
  count(highway, wt = length)
wf_2018s$year = 2018

wf_2018s


# oe_download("isle of wight") # fails
# u = oe_match("isle of wight")
# oe_download(u$url, download_directory = ".")
# iow = oe_read("geofabrik_isle-of-wight-latest.osm.pbf")
