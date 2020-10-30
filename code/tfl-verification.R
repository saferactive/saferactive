library(tidyverse)
library(sf)

counter_locations = sf::read_sf("tfl-cycle-counter-locations.geojson")
counter_locations = counter_locations %>% rename(`Site ID` = UnqID)
counter_df = readr::read_csv("raw-tfl-cycle-counter-data-2014-2019.csv")
counter_df = counter_df %>% rename(Survey_wave = `Survey wave (calendar quarter)`)

#filter daytime counts
unique(counter_df$Period)
counter_daytime = counter_df %>%
  filter(Period == "AM peak (07:00-10:00)" | Period == "PM peak (16:00-19:00)" | Period == "Inter-peak (10:00-16:00)")
unique(counter_daytime$Period)

#find year
counter_daytime$year = case_when(is.na(counter_daytime$Survey_wave) ~ lubridate::year(counter_daytime$`Survey date`), TRUE ~ as.numeric(substr(counter_daytime$Survey_wave, 1, 4)))
counter_daytime = counter_daytime %>%
  filter(! is.na(year))
dim(counter_daytime) #945728

# group by date, site and direction
counter_daily = counter_daytime %>%
  group_by(`Survey date`, `Site ID`, Direction) %>%
  summarise(
    n_counters = n(),
    total_counts = sum(`Total cycles`, na.rm = TRUE),
    mean_counts = mean(`Total cycles`, na.rm = TRUE)) %>%
  ungroup()

ccc = left_join(counter_daytime, counter_daily, by = c("Survey date", "Site ID", "Direction"))
View(ccc %>%
       filter(n_counters == 96))

View(counter_daytime)

View(counter_daily %>%
  group_by(n_counters) %>%
  tally())

View(counter_daytime %>%
       group_by(Time) %>%
       tally())

View(counter_daytime %>%
       group_by(year) %>%
       tally())

View(counter_daytime %>%
       filter(is.na(year)))

View(counter_daytime %>%
  filter(`Site ID` == "CENCY010",
         `Survey date` == "2014-03-24",
         year == 2014)
)
counter_sf = inner_join(counter_locations, counter_df)

counter_bng = counter_sf %>%
  st_transform(crs = 27700)

st_coordinates(counter_bng)[1]
