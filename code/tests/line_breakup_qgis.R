# Aim: test break-up of linestrings
# https://github.com/saferactive/saferactive/issues/54
# https://github.com/paleolimbot/qgisprocess

remotes::install_github("itsleeds/pct")
remotes::install_github("paleolimbot/qgisprocess")


library(dplyr)
library(qgisprocess)

rnet = pct::get_pct_rnet("isle-of-wight")
nrow(rnet)
rnet$length = as.numeric(sf::st_length(rnet))
summary(rnet$length)

rnet = sf::st_transform(rnet, 27700)
rnet_10 = rnet %>% top_n(n = 10, wt = length)

qgis_configure()
algorithms = qgis_algorithms()
algorithms %>% filter(grepl(pattern = "split", x = algorithm, ignore.case = TRUE))
qgis_show_help("native:splitlinesbylength")
output = qgis_run_algorithm(
  algorithm = "native:splitlinesbylength",
  INPUT = rnet_10,
  LENGTH = 500
  )

qgis_show_help("grass7:v.split")
output = qgis_run_algorithm(
  algorithm = "grass7:v.split",
  input = rnet_10,
  length = 500
)

output
rnet_10_split = sf::st_read(output[[1]][1])

sf::st_length(rnet_10)
sf::st_length(rnet_10_split)
plot(rnet_10$geometry, lwd = 9, col = "grey")
plot(sf::st_geometry(rnet_10_split), add = TRUE)

# benchmarking
system.time({
  output = qgis_run_algorithm(
    algorithm = "grass7:v.split",
    input = rnet,
    length = 500
  )
})
# user  system elapsed
# 2.159   0.996   2.661
rnet_split = sf::st_read(output[[1]][1])

rnet_split
nrow(rnet)
nrow(rnet_split)
500000 / nrow(rnet)

