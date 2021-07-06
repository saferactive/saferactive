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
browseURL(f)
d_region = readODS::read_ods(f)
View(d_region) # from looking at that we have:
first_two_rows_with_years_mentioned = c(68, 91)
d_region[[1]][first_two_rows_with_years_mentioned]
# [1] "Average miles travelled by mode, region and Rural-Urban Classification: England, 2002/2003"
# [2] "Average miles travelled by mode, region and Rural-Urban Classification: England, 2003/2004"
d_region[first_two_rows_with_years_mentioned[1] + 3, , drop = TRUE]
# [1] NA
# [[2]]
# [1] "Walk6"
#
# [[3]]
# [1] "Of which: walks of over a mile"
# ...
column_names_ods = c(
  "region",
  "walk",
  "walk_1_mile_plus",
  "bicycle",
  "car_driver",
  "car_passenger",
  "motorcycle",
  "other_private_transport",
  "bus_in_london",
  "other_local_bus",
  "non_local_bus",
  "london_underground",
  "surface_rail",
  "taxi",
  "other_public_transport",
  "all_modes",
  "all_modes_excl_short_walk",
  "wnweighted_sample"
)

names(d_region) = column_names_ods
# actual data starts 5 rows below year statement
# Years are this many rows apart:
diff(first_two_rows_with_years_mentioned)
# 23
first_row_with_data = first_two_rows_with_years_mentioned[1] + 5
# there are 9 regions in the UK
d_region[first_row_with_data:(first_row_with_data + 8), ] # works well
d_region[first_row_with_data:(first_row_with_data + 8), column_names_ods] # works well
years_of_data = 2003:2019 # data up to 2019
n_years = length(years_of_data)
row_d1s = seq(n_years) * 23 + first_row_with_data - 23
d_region[row_d1s, ]

d_region_clean = map_dfr(row_d1s, function(i) d_region[i:(i + 8), ], .id = "years_after_2002")
d_region_clean = d_region_clean %>%
  as_tibble() %>%
  mutate(across(walk:wnweighted_sample, as.numeric)) %>%
  mutate(year = as.numeric(years_after_2002) + 2002)

d_region_clean %>%
  ggplot() +
  geom_line(aes(year, bicycle, colour = region))


uk_regions = ukboundaries::lad2016_simple

