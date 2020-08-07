# Aim: get data from CID

remotes::install_github("saferactive/trafficalmr")
library(trafficalmr)
nrow(crashes_wf)
nrow(casualties_wf)
crash_summary = tc_join_stats19(crashes_wf, casualties_wf, vehicles_wf)
nrow(crash_summary)

remotes::install_github("PublicHealthDataGeek/CycleInfraLnd")
library(CycleInfraLnd)

get_cid_lines(type = "traffic_calming")

cid_traffic_calming = sf::read_sf("https://cycling.data.tfl.gov.uk/CyclingInfrastructure/data/points/traffic_calming.json")

