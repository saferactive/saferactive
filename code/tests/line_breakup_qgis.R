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
300 * 3 / 60

rnet_all = readRDS("rnet_national_sf_27700.Rds")
rnet_1pc = rnet_all %>% sample_frac(0.01)
system.time({
  output = qgis_run_algorithm(
    algorithm = "grass7:v.split",
    input = rnet_1pc,
    length = 500
  )
})
# user  system elapsed
# 3.872   2.339   4.772
# 5 * 100 / 60
rnet_split = sf::st_read(output[[1]][1])
nrow(rnet_1pc)
nrow(rnet_split)
sum(sf::st_length(rnet_1pc))
sum(sf::st_length(rnet_split))
sum(sf::st_length(rnet_split) * rnet_split$bicycle)
sum(sf::st_length(rnet_1pc) * rnet_1pc$bicycle) # the same

# run on national dataset
system.time({
  output = qgis_run_algorithm(
    algorithm = "grass7:v.split",
    input = rnet_all,
    length = 500
  )
})
# user  system elapsed
# 213.813 204.001 297.571
rnet_split = sf::st_read(output[[1]][1])
nrow(rnet_split) / nrow(rnet_all)
# [1] 1.374662
sf::st_crs(rnet_split) = 27700
saveRDS(rnet_split, "rnet_split.Rds")
