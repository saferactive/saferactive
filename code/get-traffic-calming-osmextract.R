# Aim: get traffic calming interventions for london

remotes::install_github("itsleeds/osmextract")
remotes::install_github("saferactive/trafficalmr")
library(trafficalmr)
library(osmextract)

query = "SELECT * FROM 'lines' WHERE highway IN ('cycleway')"
london_cycleway = oe_get("Greater London", extra_tags = c("emergency"), query = query)
nrow(london_cycleway)
table(london_cycleway$emergency) # 65 emergency points in London

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


