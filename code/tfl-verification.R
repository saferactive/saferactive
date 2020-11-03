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

############# Weather
doubles = counter_daytime %>%
  group_by(`Site ID`, Direction, year, Time, Location, Survey_wave) %>%
  summarise(n_repeats = n()) %>%
  ungroup()

weather_repeats = doubles %>%
  filter(n_repeats == 2)

remove5 = weather_repeats %>%
  inner_join(counter_daytime, by = c("Site ID" = "Site ID", "Direction" = "Direction", "year" = "year", "Time" = "Time", "Location" = "Location", "Survey_wave" = "Survey_wave")) %>%
    filter(Weather == "Dry")

counter_daytime = counter_daytime %>%
  filter(! id %in% remove5$id)
dim(counter_daytime) #928816

###################

# group by survey wave, site and direction
# select only sites which have a full 12 hours (48 periods) of survey data (this can be across several survey dates but must be within the same survey wave/quarter)
counter_daily = counter_daytime %>%
  group_by(`Site ID`, Direction, year, Survey_wave) %>%
  summarise(
    n_counters = n(),
    total_counts = sum(`Total cycles`, na.rm = TRUE),
    mean_counts = mean(`Total cycles`, na.rm = TRUE)) %>%
  ungroup()

counter_clean = left_join(counter_daytime, counter_daily, by = c("Site ID", "Direction", "year", "Survey_wave")) %>%
  filter(n_counters == 48)
dim(counter_clean) #919440

# CENCY201 Tooley Street Survey wave changes while Survey date stays the same
# not enough hours only covers 7am - 2pm

# View(counter_clean %>%
#   group_by(n_counters) %>%
#   tally())

# unique(counter_clean$year)
# unique(substr(counter_clean$Survey_wave,6,7))
# unique(substr(counter_clean$`Survey date`,6,7))

# counter_daytime %>%
#   filter(substr(counter_daytime$Survey_wave, 6, 7) == "Q1")

# Clean the Time strings
counter_clean$Time = gsub("0-0", "0 - 0", counter_clean$Time)
counter_clean$Time = gsub("5-0", "5 - 0", counter_clean$Time)
counter_clean$Time = gsub("0-1", "0 - 1", counter_clean$Time)
counter_clean$Time = gsub("5-1", "5 - 1", counter_clean$Time)

View(counter_clean %>%
       group_by(Time) %>%
       tally())

View(counter_clean %>%
       group_by(year) %>%
       tally())

# View(counter_clean %>%
#   filter(`Site ID` == "CENCY010",
#          `Survey date` == "2014-03-24",
#          year == 2014)
# )




# Get location data and create sf object ----------------------------------

counter_sf = inner_join(counter_locations, counter_df)

counter_bng = counter_sf %>%
  st_transform(crs = 27700)

st_coordinates(counter_bng)[1]
