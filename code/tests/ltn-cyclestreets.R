# Aim: get data from cyclestreets on ltns

u = "https://api.cyclestreets.net/v2/advocacydata.ltns?bbox=0.101131,52.195807,0.170288,52.209719&zoom=15"
browseURL(u) # requires api key

# try with API key
?cyclestreets::journey
from = c(-1.55, 53.80) # geo_code("leeds")
to = c(-1.76, 53.80) # geo_code("bradford uk")
library(cyclestreets)
r1 = journey(from, to, silent = FALSE)
u = paste0("https://api.cyclestreets.net/v2/advocacydata.ltns?key=",
  Sys.getenv("CYCLESTREETS"),
  "&bbox=0.101131,52.195807,0.170288,52.209719&zoom=15")
browseURL(u)
ltn_data = sf::read_sf(u)
mapview::mapview(ltn_data["ratrun"])
