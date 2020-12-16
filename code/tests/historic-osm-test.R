# Get and analyse historic OSM data - starter for 10

# install up-to-date osmextract
remotes::install_github("itsleeds/osmextract", "add_normalizepath")
# remotes::install_github("itsleeds/osmextract")
library(osmextract)
library(tidyverse)
library(sf)

osm_directory = "/home/bananafan/Downloads/"
osm_directory = "/home/robin/hd/data/osm/internal/"
osm_directory = "~/hd/data/osm/internal/"

# file.copy(file.path(osm_directory, "greater-london-160101-internal.osm.pbf"), ".")
# fails:
# lnd_2016 = osmextract::oe_read(file_path = "~/hd/data/osm/internal/greater-london-160101-internal.osm.pbf", layer = "lines")
# lnd_2016 = osmextract::oe_read(file_path = "greater-london-160101-internal.osm.pbf", layer = "lines")
# ?oe_read
f = file.path(osm_directory, "greater-london-160101-internal.osm.pbf")
extra_tags = c("maxspeed", "bicycle", "traffic_calming", "oneway", "lanes", "sidewalk", "ref", "lit", "foot", "motor_vehicle")
lnd_2016 = osmextract::oe_read(file_path = f, layer = "lines", extra_tags = extra_tags)
lnd_2016$length = as.numeric(sf::st_length(lnd_2016))
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
# f = file.path(osm_directory, "greater-london-180101.osm.pbf")
f = file.path(osm_directory, "greater-london-180101-internal.osm.pbf")
lnd_2018 = osmextract::oe_read(file_path = f, layer = "lines", extra_tags = extra_tags)
lnd_2018$length = as.numeric(sf::st_length(lnd_2018))

wf_2018 = lnd_2018[wf_redbridge, , op = st_within]
dim(wf_2018)
# [1] 17114    21

wf_2018 %>% sample_n(1000) %>% mapview::mapview()
table(wf_2018$traffic_calming)
# cushion 25

wf_2018j = wf_2018 %>%
  st_join(x = ., y = wf_redbridge["NAME"])
dim(wf_2018j) # bingo columns = 22, more data
wf_2018s = wf_2018j %>%
  sf::st_drop_geometry() %>%
  group_by(NAME) %>%
  count(highway, wt = length)
wf_2018s$year = 2018

wf_yrs = rbind(wf_2018s, wf_2016s)
wf_yrs$year = as.character(wf_yrs$year)
sort(table(wf_2018j$)

     https://us02web.zoom.us/s/86909837868?pwd=TEM2bkhicnVBUEVrZlg1WUxOK1VYZz09#success

ggplot(wf_yrs %>% filter(stringr::str_detect(string = highway, pattern = "cycle|res"))) +
  geom_bar(aes(year, n, fill = year), stat = "identity") +
  facet_grid(NAME ~ highway)

ggplot(wf_yrs) +
  geom_bar(aes(year, n, fill = year), stat = "identity") +
  facet_grid(NAME ~ maxspeed)

wf_percent = wf_yrs %>%
  group_by(NAME, highway) %>%
  summarise(
    relative_change = (percent_change = .data$n[.data$year == 2018] / .data$n[.data$year == 2016]),
    percent_change = (relative_change - 1) * 100
    )
wf_percent
ggplot(wf_percent %>% filter(stringr::str_detect(string = highway, pattern = "cycle|res"))) +
  geom_bar(aes(highway, percent_change), stat = "identity") +
  facet_grid(~NAME)
# oe_download("isle of wight") # fails
# u = oe_match("isle of wight")
# oe_download(u$url, download_directory = ".")
# iow = oe_read("geofabrik_isle-of-wight-latest.osm.pbf")
