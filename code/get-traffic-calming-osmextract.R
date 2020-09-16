# Aim: get traffic calming interventions for london

remotes::install_github("itsleeds/osmextract")
remotes::install_github("saferactive/trafficalmr")
library(trafficalmr)
library(osmextract)
library(tidyverse)

library(trafficalmr)

tc_london = tc_traffic_calming(place = "Greater London")
table(sf::st_geometry_type(tc_london)) # around 20% are linestrings
# GEOMETRY              POINT         LINESTRING
# 0               4869                970
nrow(tc_london)
# [1] 5839
table(tc_london$traffic_calming)
# bump    bump;choker        chicane         choker         choler
# 1071              9             31            270              2
# cushion           hump         island             no           none
# 1446           1177            267              2              6
# painted_table   rumble_strip speed_cushions          table            yes
# 2             16              1           1461             78
tc_london$geometry_type = as.character(sf::st_geometry_type(tc_london))
tc_london_lines = tc_london %>% filter(geometry_type == "LINESTRING")
table(tc_london_lines$traffic_calming)
# bump        chicane         choker        cushion           hump
# 30              3             52            582             62
# island             no           none   rumble_strip speed_cushions
# 12              2              6              3              1
# table            yes
# 178             39



tc_osmdata = sf::read_sf("traffic_calming_osm_london.geojson")
nrow(tc_osmdata)
# [1] 12354
table(tc_osmdata$traffic_calming) # fewer named ones...
sum(!is.na(tc_osmdata$traffic_calming))
# [1] 5145
summary(as.factor(tc_osmdata$traffic_calming))
tc_osmdata_na = tc_osmdata %>% filter(is.na(traffic_calming))
mapview::mapview(tc_osmdata[1:99, ])

# get emergency service access lines --------------------------------------

query = "SELECT * FROM 'lines' WHERE highway IN ('cycleway')"
london_cycleway = oe_get("Greater London", extra_tags = c("emergency"), query = query)
nrow(london_cycleway)
table(london_cycleway$emergency) # 65 emergency points in London
emergency = london_cycleway %>%
  filter(emergency == "yes")
mapview::mapview(emergency)

?osmextract::oe_get
# test query
query = "SELECT DISTINCT highway FROM 'lines'"
osmextract::oe_get("Greater London", query = query)

query = "SELECT * FROM 'lines' WHERE highway IN ('primary')"
primary = oe_get("Greater London", query = query)
table(primary$highway)
plot(primary$geometry)

query = "SELECT * FROM 'lines' WHERE emergency IN ('yes')"
emergency = oe_get("Greater London", query = query)
table(primary$highway)
plot(primary$geometry)


f = osmextract::oe_find("Greater London")[2]
# fails with message:
# Error in CPL_read_ogr(dsn, layer, query, as.character(options), quiet,  :
#                         Not compatible with STRSXP: [type=closure].
emergency_service_points = osmextract::oe_get(
  place = "Greater London",
  layer = "lines",
  query = q,
  force_vectortranslate = TRUE
  )
# fails
emergency_service_points = osmextract::oe_read(file_path = f, query = q)
query = "SELECT * FROM 'lines' WHERE emergency IN ('yes')"
emergency = sf::st_read(f, query = q, layer = "lines")


