library(tidyverse)
library(sf)

counter_locations = sf::read_sf("tfl-cycle-counter-locations.geojson")
counter_locations = counter_locations %>% rename(`Site ID` = UnqID)
counter_df = readr::read_csv("raw-tfl-cycle-counter-data-2014-2019.csv")

#group by date
counter_df$year = lubridate::year(counter_df$`Survey date`)
counter_daily = counter_df %>%
  group_by(`Survey date`, `Site ID`) %>%
  summarise(
    n_counters = n(),
    total_counts = sum(`Total cycles`, na.rm = TRUE),
    mean_counts = mean(`Total cycles`, na.rm = TRUE))

counter_sf = inner_join(counter_locations, counter_df)

counter_bng = counter_sf %>%
  st_transform(crs = 27700)

st_coordinates(counter_bng)[1]
