library(tidyverse)
library(sf)

counter_locations = sf::read_sf("tfl-cycle-counter-locations.geojson")
counter_locations = counter_locations %>% rename(`Site ID` = UnqID)
counter_df = readr::read_csv("raw-tfl-cycle-counter-data-2014-2019.csv")
counter_df = counter_df %>% rename(Survey_wave = `Survey wave (calendar quarter)`)
dim(counter_df) #1264064

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
  group_by(`Survey date`, `Site ID`, Direction, year) %>%
  summarise(
    n_counters = n(),
    total_counts = sum(`Total cycles`, na.rm = TRUE),
    mean_counts = mean(`Total cycles`, na.rm = TRUE)) %>%
  ungroup()

ccc = left_join(counter_daytime, counter_daily, by = c("Survey date", "Site ID", "Direction", "year"))
dim(ccc %>%
       filter(n_counters > 56))

# CENCY201 Tooley Street Survey wave changes while Survey date stays the same

# Select the correct survey wave only

#create ID variable
counter_daytime$id = rownames(counter_daytime)

# remove rows where survey wave doesn't match survey date
remove1 = counter_daytime %>%
      filter(substr(counter_daytime$Survey_wave,6,7) == "Q1" & !((substr(counter_daytime$`Survey date`,6,7) == "01") | (substr(counter_daytime$`Survey date`,6,7) == "02") | (substr(counter_daytime$`Survey date`,6,7) == "03")))

counter_daytime = counter_daytime %>%
  filter(! id %in% remove1$id)

remove2 = counter_daytime %>%
      filter(substr(counter_daytime$Survey_wave,6,7) == "Q2" & !((substr(counter_daytime$`Survey date`,6,7) == "04") | (substr(counter_daytime$`Survey date`,6,7) == "05") | (substr(counter_daytime$`Survey date`,6,7) == "06")))

counter_daytime = counter_daytime %>%
  filter(! id %in% remove2$id)

remove3 = counter_daytime %>%
      filter(substr(counter_daytime$Survey_wave,6,7) == "Q3" & !((substr(counter_daytime$`Survey date`,6,7) == "07") | (substr(counter_daytime$`Survey date`,6,7) == "08") | (substr(counter_daytime$`Survey date`,6,7) == "09")))

counter_daytime = counter_daytime %>%
  filter(! id %in% remove3$id)

remove4 = counter_daytime %>%
      filter(substr(counter_daytime$Survey_wave,6,7) == "Q4" & !((substr(counter_daytime$`Survey date`,6,7) == "10") | (substr(counter_daytime$`Survey date`,6,7) == "11") | (substr(counter_daytime$`Survey date`,6,7) == "12")))

counter_daytime = counter_daytime %>%
  filter(! id %in% remove4$id)

dim(counter_daytime) #929008

#############
doubles = counter_daytime %>%
  group_by(`Survey date`, `Site ID`, Direction, year, Time, Location, Survey_wave) %>%
  summarise(n_waves = n())

doubles %>%
  group_by(n_waves) %>%
  tally()

View(doubles %>%
  filter(n_waves == 2))

unique(substr(ccc$Survey_wave,6,7))
unique(substr(ccc$`Survey date`,6,7))

dim(counter_daytime[!is.na(substr(counter_daytime$`Survey date`,6,7) == "01"),])
dim(counter_daytime[is.na(substr(counter_daytime$Survey_wave, 6, 7) == "F1") == FALSE,])
dim(counter_daytime[substr(counter_daytime$`Survey date`,6,7) == "02",])

counter_daytime %>%
  filter(substr(counter_daytime$Survey_wave, 6, 7) == "Q1")



View(ccc %>%
       filter(`Site ID` == "INNCY570", Time == "1000 - 1015"))

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
