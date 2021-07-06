# Aim get estimates of cycling uptake regionally from the NTS's summary datasets

install.packages("ows4R")
library(ows4R)
library(sf)
library(tidyverse)
library(tmap)
tmap_mode("view")

# See https://github.com/saferactive/saferactive/issues/76
# data from https://geoportal.statistics.gov.uk/datasets/regions-december-2020-en-bgc/explore?location=0.000000%2C0.000000%2C0.00
# piggyback::pb_upload(f2)
# pb_list_df = piggyback::pb_list()
# pb_list_df %>%
#   filter(str_detect(file_name, "gion"))
#
# piggyback::pb_download_url("Regions_.December_2020._EN_BGC.geojson")
regions = read_sf("https://github.com/saferactive/saferactive/releases/download/0.1.4/Regions_.December_2020._EN_BGC.geojson")
regions = sf::st_make_valid(regions)
qtm(regions)
regions_simple = rmapshaper::ms_simplify(regions, 0.01)
qtm(regions_simple)

u = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/905969/nts9904.ods"
f = basename(u)
if(!file.exists(f)) download.file(u, f)
d_region = readODS::read_ods(f)
View(d_region) # from looking at that we have:
first_two_rows_with_years_mentioned = c(68, 91)
d_region[[1]][first_two_rows_with_years_mentioned]
# [1] "Average miles travelled by mode, region and Rural-Urban Classification: England, 2002/2003"
# [2] "Average miles travelled by mode, region and Rural-Urban Classification: England, 2003/2004"

uk_regions = ukboundaries::lad2016_simple

