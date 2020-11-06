library(tidyverse)
library(sf)

counter_locations = sf::read_sf("tfl-cycle-counter-locations.geojson")
counter_locations = counter_locations %>% rename(`Site ID` = UnqID)
counter_df = readr::read_csv("raw-tfl-cycle-counter-data-2014-2019.csv")
counter_df = counter_df %>% rename(Survey_wave = `Survey wave (calendar quarter)`)
dim(counter_df) #1264064

# saveRDS(counter_df, "counter_df.Rds")

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
    n_periods = n(),
    total_daily = sum(`Total cycles`, na.rm = TRUE),
    mean_daily = mean(`Total cycles`, na.rm = TRUE)) %>%
  ungroup()

counter_clean = left_join(counter_daytime, counter_daily, by = c("Site ID", "Direction", "year", "Survey_wave")) %>%
  filter(n_periods == 48)
dim(counter_clean) #919440

# CENCY201 Tooley Street Survey wave changes while Survey date stays the same
# not enough hours only covers 7am - 2pm

# View(counter_clean %>%
#   group_by(n_periods) %>%
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

# Remove 2014 counts
counter_clean = counter_clean %>%
  filter(year != 2014)
dim(counter_clean) #846528

# saveRDS(counter_clean, "counter_clean.Rds")

# Get location data and create sf object ----------------------------------

counter_clean = readRDS("counter_clean.Rds")

counter_sf = inner_join(counter_locations, counter_clean)

counter_bng = counter_sf %>%
  st_transform(crs = 27700)

# st_coordinates(counter_bng)[1]


# Calculate change in TfL counts ------------------------------------------

counter_change = counter_bng %>%
  group_by(`Site ID`, Direction) %>%
  mutate(mean_site_direction = mean(total_daily)) %>%
  ungroup() %>%
  filter(mean_site_direction > 0) %>% # remove counts where there has never been a cyclist
  mutate(change_cycles = total_daily/mean_site_direction,
         site_direction = paste(`Site ID`, Direction))
sum(is.na(counter_change$change_cycles))/nrow(counter_change) #0
dim(counter_change) #846528

# counter_change$site_direction = paste(counter_change$`Site ID`, counter_change$Direction)

#Find most common weather type through the day
# counter_change = counter_change %>%
#   group_by(year, Survey_wave, `Site ID`, Direction) %>%
#   mutate(main_weather =

# Daily totals instead of 15 minute periods
counter_days = counter_change %>%
  select(year, Survey_wave, Borough, `Site ID`, Direction, site_direction, ProgID, Location, total_daily, mean_daily, mean_site_direction, change_cycles) %>%
  unique()
dim(counter_days) #17636

dim(counter_days %>% filter(ProgID == "CENCY")) #7218
dim(counter_days %>% filter(ProgID == "INNCY")) #5924
dim(counter_days %>% filter(ProgID == "OUTCY")) #4494

dim(counter_days %>% filter(ProgID == "CENCY") %>% select(site_direction) %>% unique()) #420
dim(counter_days %>% filter(ProgID == "INNCY") %>% select(site_direction) %>% unique()) #1202
dim(counter_days %>% filter(ProgID == "OUTCY") %>% select(site_direction) %>% unique()) #906


# Survey wave corrections -------------------------------------------------

# Group by site_direction and year so Central London counts that have been surveyed 4 times a year don't have 4 times more influence than Inner/Outer London counts
#Correct central london counts for varying numbers in each survey wave

# counter_days %>% filter(ProgID == "CENCY") %>%
#   group_by(Survey_wave) %>%
#   tally()

# get (weighted) mean values within each wave
counter_wave = counter_days %>%
  filter(ProgID == "CENCY") %>%
  group_by(year, Survey_wave, Borough, ProgID) %>%
  summarise(
    total_daily = mean(total_daily),
    change_cycles = weighted.mean(change_cycles, w = mean_site_direction, na.rm = TRUE)
  ) %>%
  ungroup()

# get mean values across the waves in each year
counter_wave = counter_wave %>%
  group_by(year, Borough) %>%
  mutate(mean_for_year = mean(total_daily),
         change_for_year = mean(change_cycles),
         wave_correction_mean = total_daily/mean_for_year,
         wave_correction_change = change_cycles/change_for_year) %>%
  ungroup()

counter_year_central = counter_wave %>%
  group_by(year, Borough) %>%
  summarise(mean_for_year = mean(mean_for_year),
            change_for_year = mean(change_for_year))

# Correct for missing survey wave 4 in 2019
wave_corrections = counter_wave %>%
  mutate(wave_number = substr(Survey_wave, 6, 7)) %>%
  group_by(wave_number) %>%
  summarise(mean_wave_correction = mean(wave_correction_mean),
            change_wave_correction = mean(wave_correction_change))

counter_year_central$mean_for_year[counter_year_central$year == 2019] = counter_year_central$mean_for_year[counter_year_central$year == 2019] / mean(c(wave_corrections[[1,2]],wave_corrections[[2,2]],wave_corrections[[3,2]]))

counter_year_central$change_for_year[counter_year_central$year == 2019] = counter_year_central$change_for_year[counter_year_central$year == 2019] / mean(c(wave_corrections[[1,3]],wave_corrections[[2,3]],wave_corrections[[3,3]]))

View(counter_year_central)

# Join back together with inner and outer london counts






system.time({counter_year = counter_days %>%
  group_by(year, site_direction, Borough, ProgID, mean_site_direction) %>%
  summarise(
    total_daily = mean(total_daily, na.rm = TRUE),
    change_cycles = mean(change_cycles)) %>%
  ungroup()})

dim(counter_year %>% filter(ProgID == "CENCY")) #2026
dim(counter_year %>% filter(ProgID == "INNCY")) #5924
dim(counter_year %>% filter(ProgID == "OUTCY")) #4494

# Group by year and borough
counter_means_year = counter_year %>%
  st_drop_geometry() %>%
  group_by(year, Borough) %>%
  summarise(
    n_counters = length(unique(site_direction)),
    borough_sum = sum(total_daily, na.rm = TRUE),
    borough_mean = mean(total_daily, na.rm = TRUE),
    change_tfl_cycles = weighted.mean(change_cycles, w = mean_site_direction, na.rm = TRUE)
  )

counter_means_2015 = counter_means_year %>%
  filter(year == 2015) %>%
  ungroup() %>%
  mutate(borough_mean_2015 = borough_mean) %>%
  select(Borough, borough_mean_2015)


counter_la_results = inner_join(counter_means_year, counter_means_2015) %>%
  mutate(relative_to_2015 = borough_mean / borough_mean_2015)
# counter_la_results$Borough = gsub("&", "and", counter_la_results$Borough)

ggplot(counter_la_results) +
  geom_line(aes(year, relative_to_2015, colour = Borough))

readr::write_csv(counter_la_results, "tfl-counter-results-london-boroughs-2015-2019.csv")
piggyback::pb_upload("tfl-counter-results-london-boroughs-2015-2019.csv", repo = "itsleeds/saferroadsmap")

lads = spData::lnd %>% rename(Borough = NAME) %>%
  mutate(Borough = as.character(Borough)) %>%
  mutate(Name = abbreviate(Borough, minlength = 2))
lads$Borough[!lads$Borough %in% counter_la_results$Borough]
counter_means_2015$Borough[!counter_means_2015$Borough %in% lads$Borough]
lads = lads %>%
  mutate(Borough = str_replace(string = Borough, pattern = " and", replacement = " &"))
lads$Borough[!lads$Borough %in% counter_la_results$Borough]
lads_data = inner_join(lads, counter_la_results)


library(tmap)
tm_shape(lads_data) +
  tm_polygons("relative_to_2015", palette = "BrBG", n = 6) +
  tm_text(text = "Name", size = 0.7) +
  tm_facets("year")

# Average across the 5 years
counter_multiyear = lads_data %>%
  group_by(Name) %>%
  summarise(borough_mean = mean(borough_mean))

tm_shape(lads_data) +
  tm_polygons("borough_mean", palette = "BrBG", n = 6, style = "jenks") +
  tm_text(text = "Name", size = 0.7)
