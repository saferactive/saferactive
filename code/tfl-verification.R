library(tidyverse)
library(sf)

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
#
# View(counter_clean %>%
#        group_by(Time) %>%
#        tally())
#
View(counter_clean %>%
       group_by(Survey_wave) %>%
       tally())

# View(counter_clean %>%
#   filter(`Site ID` == "CENCY010",
#          `Survey date` == "2014-03-24",
#          year == 2014)
# )

# Remove 2014 counts
# Full 2014 data is available for Central London (all 4 survey waves) but no 2014 data is available for the Inner or Outer London locations
counter_clean = counter_clean %>%
  filter(year != 2014)
dim(counter_clean) #846528

# saveRDS(counter_clean, "counter_clean.Rds")

# Get location data and create sf object ----------------------------------

counter_clean = readRDS("counter_clean.Rds")

counter_locations = sf::read_sf("tfl-cycle-counter-locations.geojson")
counter_locations = counter_locations %>% rename(`Site ID` = UnqID)
counter_locations$Borough = gsub("&", "and", counter_locations$Borough)


counter_nogeo = counter_locations %>%
  st_drop_geometry %>%
  inner_join(counter_clean)

counter_sf = inner_join(counter_locations, counter_clean)

counter_bng = counter_sf %>%
  st_transform(crs = 27700)
# st_coordinates(counter_bng)[1]


# Daily totals instead of 15 minute periods
counter_days = counter_nogeo %>%
  select(year, Survey_wave, Borough, `Site ID`, Direction, ProgID, Location, total_daily, mean_daily) %>%
  unique()
dim(counter_days) #17636

# Use annual equivalent adjustment factors taken from TfL Central London grid survey data to account for seasonal variation
# Also helps to correct central london counts for varying numbers in each survey wave (Corrects for missing survey wave 4 in 2019)
season_adjust = counter_days %>%
  mutate(wave_number = substr(Survey_wave, 6, 7),
         adjusted_total = case_when(wave_number == "Q1" ~ total_daily * 1.14,
                                    wave_number == "Q2" ~ total_daily * 0.93,
                                    wave_number == "Q3" ~ total_daily * 0.91,
                                    wave_number == "Q4" ~ total_daily * 1.06,
                                    TRUE ~ total_daily * 0.93)) # inner and outer London data were surveyed in Q2

# Calculate change in TfL counts ------------------------------------------

counter_change = season_adjust %>%
  group_by(`Site ID`, Direction) %>%
  mutate(mean_site_direction = mean(adjusted_total)) %>%
  ungroup() %>%
  filter(mean_site_direction > 0) %>% # remove counts where there has never been a cyclist
  mutate(change_cycles = adjusted_total/mean_site_direction,
         site_direction = paste(`Site ID`, Direction))
sum(is.na(counter_change$change_cycles))/nrow(counter_change) #0


# counter_change$site_direction = paste(counter_change$`Site ID`, counter_change$Direction)

#Find most common weather type through the day
# counter_change = counter_change %>%
#   group_by(year, Survey_wave, `Site ID`, Direction) %>%
#   mutate(main_weather =


dim(counter_change %>% filter(ProgID == "CENCY")) #7218
dim(counter_change %>% filter(ProgID == "INNCY")) #5924
dim(counter_change %>% filter(ProgID == "OUTCY")) #4494

dim(counter_change %>% filter(ProgID == "CENCY") %>% select(site_direction) %>% unique()) #420
dim(counter_change %>% filter(ProgID == "INNCY") %>% select(site_direction) %>% unique()) #1202
dim(counter_change %>% filter(ProgID == "OUTCY") %>% select(site_direction) %>% unique()) #906

# Survey wave corrections -------------------------------------------------
# Group by site_direction and year so Central London counts that have been surveyed 4 times a year don't have 4 times more influence than Inner/Outer London counts

# get mean values for each site_location and year
counter_year = counter_change %>%
  group_by(year, Borough, `Site ID`, Direction, site_direction, ProgID, Location, mean_site_direction) %>%
  summarise(
    total_daily = mean(total_daily),
    adjusted_total = mean(adjusted_total),
    change_cycles = mean(change_cycles)
  ) %>%
  ungroup()
dim(counter_year) #12444


dim(counter_year %>% filter(ProgID == "CENCY")) #2026
dim(counter_year %>% filter(ProgID == "INNCY")) #5924
dim(counter_year %>% filter(ProgID == "OUTCY")) #4494



saveRDS(counter_year, "tfl-counts-by-site.Rds")



# Group by borough --------------------------------------------------------


counter_means_year = counter_year %>%
  group_by(year, Borough) %>%
  summarise(
    n_counters = length(unique(site_direction)),
    borough_mean = mean(adjusted_total, na.rm = TRUE),
    change_tfl_cycles = weighted.mean(change_cycles, w = mean_site_direction, na.rm = TRUE)
  )

counter_means_2015 = counter_means_year %>%
  filter(year == 2015) %>%
  ungroup() %>%
  mutate(borough_mean_2015 = borough_mean,
         borough_change_2015 = change_tfl_cycles) %>%
  select(Borough, borough_mean_2015, borough_change_2015)


counter_la_results = inner_join(counter_means_year, counter_means_2015) %>%
  mutate(count_relative_to_2015 = borough_mean / borough_mean_2015,
         change_relative_to_2015 = change_tfl_cycles / borough_change_2015)
# counter_la_results$Borough = gsub("&", "and", counter_la_results$Borough)

ggplot(counter_la_results) +
  geom_line(aes(year, change_tfl_cycles, colour = Borough))

readr::write_csv(counter_la_results, "tfl-counter-results-london-boroughs-2015-2019.csv")
piggyback::pb_upload("tfl-counter-results-london-boroughs-2015-2019.csv", repo = "itsleeds/saferroadsmap")

lads = spData::lnd %>% rename(Borough = NAME) %>%
  mutate(Borough = as.character(Borough)) %>%
  mutate(Name = abbreviate(Borough, minlength = 2))
# lads$Borough[!lads$Borough %in% counter_la_results$Borough]
# counter_means_2015$Borough[!counter_means_2015$Borough %in% lads$Borough]
# lads = lads %>%
#   mutate(Borough = str_replace(string = Borough, pattern = " and", replacement = " &"))
# lads$Borough[!lads$Borough %in% counter_la_results$Borough]
lads_data = inner_join(lads, counter_la_results)



library(tmap)
tm_shape(lads_data) +
  tm_polygons("count_relative_to_2015", palette = "BrBG", n = 6) +
  tm_text(text = "Name", size = 0.7) +
  tm_facets("year")

# Average across the 5 years
counter_multiyear = lads_data %>%
  group_by(Name) %>%
  summarise(borough_mean = mean(borough_mean))

tm_shape(counter_multiyear) +
  tm_polygons("borough_mean", palette = "BrBG", n = 6, style = "jenks") +
  tm_text(text = "Name", size = 0.7)

counter_earlyyear = lads_data %>%
  filter(year %in% 2015:2016) %>%
  group_by(Borough, Name) %>%
  summarise(borough_mean = mean(borough_mean),
            change_tfl_cycles = mean(change_tfl_cycles)) %>%
  mutate(years = "2015-16")

tm_shape(counter_earlyyear) +
  tm_polygons("borough_mean", palette = "BrBG", n = 6, style = "jenks") +
  tm_text(text = "Name", size = 0.7)

counter_lateyear = lads_data %>%
  filter(year %in% 2017:2019) %>%
  group_by(Borough, Name) %>%
  summarise(borough_mean = mean(borough_mean),
            change_tfl_cycles = mean(change_tfl_cycles)) %>%
  mutate(years = "2017-19")

tm_shape(counter_lateyear) +
  tm_polygons("borough_mean", palette = "BrBG", n = 6, style = "jenks") +
  tm_text(text = "Name", size = 0.7)

tfl_early_v_late = rbind(counter_earlyyear, counter_lateyear)


# Grouping the boroughs ---------------------------------------------------

# loosely based on London Assembly constituencies

# City, Camden, Islington
# Westminster, Kensington and Chelsea, Hammersmith and Fulham
# Southwark, Lambeth
# Lewisham, Greenwich
# Tower Hamlets, Newham
# Hackney, Waltham Forest
# Wandsworth, Merton
# Haringey, Enfield, Barnet
# Harrow, Brent
# Ealing, Hillingdon
# Hounslow, Richmond upon Thames, Kingston updon Thames
# Croydon, Sutton
# Bromley, Bexley
# Havering, Barking and Dagenham, Redbridge


# GAM model for TfL counts ------------------------------------------------

counter_bam = inner_join(counter_locations, counter_year) %>%
  st_transform(crs = 27700)
counter_bam$easting = st_coordinates(counter_bam)[,1]
counter_bam$northing = st_coordinates(counter_bam)[,2]

library(mgcv)

M = list(c(1, 0.5), NA)

m = bam(change_cycles ~
          s(year, bs = "cr", k = 5)
        + s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5))
        + ti(easting, northing, year, d = c(2,1), bs = c('ds','cr'), m = M, k = c(25, 3))
        ,
        weights = (mean_site_direction),
        family = scat,
        data = counter_bam, method = 'fREML',
        nthreads = 4, discrete = TRUE)

summary(m)
plot(m, pages = 4, scheme = 2, shade = TRUE)

m2 = bam(adjusted_total ~
          s(year, bs = "cr", k = 5)
        + s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5))
        + ti(easting, northing, year, d = c(2,1), bs = c('ds','cr'), m = M, k = c(25, 3))
        ,
        family = scat,
        data = counter_bam, method = 'fREML',
        nthreads = 4, discrete = TRUE)


summary(m2)
plot(m2, pages = 4, scheme = 2, shade = TRUE)

# Compare TfL counts with DfT GAM predictions -----------------------------------------

counter_la_results = read.csv("tfl-counter-results-london-boroughs-2015-2019.csv")

gam_preds = readRDS("pred-borough-change-cycles.Rds")

verify = left_join(counter_la_results, gam_preds, by = c("year", "Borough" = "Name"))

cor(verify$change_tfl_cycles, verify$borough_change_cycles)^2 #R squared = 0.00052
plot(x = verify$change_tfl_cycles, y = verify$borough_change_cycles, xlab = "TfL change in cycle counts (adjusted daily flow)", ylab = "GAM predictions of change (AADF)")
# ggsave(plot = tosave, "figures/gam-change-comparison.png")

verify = verify %>%
  filter(year != 2015)
dim(verify)
cor(verify$count_relative_to_2015, verify$pred_relative_to_2015)^2 #R squared = 0.0019
plot(x = verify$count_relative_to_2015, y = verify$pred_relative_to_2015, xlab = "TfL counts relative to 2015", ylab = "GAM predictions of change relative to 2015")
# ggsave(plot = tosave, "figures/change-relative-to-2015.png")


# Correlation between TfL and DfT counts (by borough) ---------------------

piggyback::pb_download("dft-counts-by-borough.Rds")
dft_counts_by_borough = readRDS("dft-counts-by-borough.Rds")

verify_raw = left_join(counter_la_results, dft_counts_by_borough, by = c("year", "Borough" = "name"))
dim(verify_raw)

# Absolute counts
cor(verify_raw$borough_mean, verify_raw$pedal_cycles)^2 #R squared = 0.799
plot(verify_raw$borough_mean, verify_raw$pedal_cycles, xlab = "Mean TfL counts (flow per 15 minutes)", ylab = "Mean AADF from DfT counts")
text(verify_raw$borough_mean, verify_raw$pedal_cycles, verify_raw$year, pos = 1, cex = 0.7)
text(verify_raw$borough_mean, verify_raw$pedal_cycles, verify_raw$Borough, pos = 1, cex = 0.7)

# Raw change
cor(verify_raw$change_tfl_cycles, verify_raw$change_cycles)^2 #R squared = 0.0008
plot(verify_raw$change_tfl_cycles, verify_raw$change_cycles, xlab = "Mean change in TfL counts (adjusted daily flow)", ylab = "Mean change in DfT counts (AADF)")
text(verify_raw$change_tfl_cycles, verify_raw$change_cycles, verify_raw$year, pos = 1, cex = 0.7)
text(verify_raw$change_tfl_cycles, verify_raw$change_cycles, verify_raw$Borough, pos = 1, cex = 0.7)


# Two time periods 2015-16 and 2017-19 ----------------------------------

dft_earlyyear = dft_counts_by_borough %>%
  filter(year %in% 2015:2016) %>%
  group_by(name) %>%
  summarise(pedal_cycles = mean(pedal_cycles),
            change_cycles = mean(change_cycles)) %>%
  mutate(years = "2015-16")

dft_lateyear = dft_counts_by_borough %>%
  filter(year %in% 2017:2019) %>%
  group_by(name) %>%
  summarise(pedal_cycles = mean(pedal_cycles),
            change_cycles = mean(change_cycles)) %>%
  mutate(years = "2017-19")

dft_early_v_late = rbind(dft_earlyyear, dft_lateyear)

verify_two = left_join(tfl_early_v_late, dft_early_v_late, by = c("years", "Borough" = "name"))
dim(verify_two)

# Absolute counts
cor(verify_two$borough_mean, verify_two$pedal_cycles)^2 #R squared = 0.826
plot(verify_two$borough_mean, verify_two$pedal_cycles, xlab = "Mean TfL counts (flow per 15 minutes)", ylab = "Mean AADF from DfT counts")
text(verify_two$borough_mean, verify_two$pedal_cycles, verify_two$years, pos = 1, cex = 0.7)
text(verify_two$borough_mean, verify_two$pedal_cycles, verify_two$Borough, pos = 1, cex = 0.7)

# Raw change
cor(verify_two$change_tfl_cycles, verify_two$change_cycles)^2 #R squared = 0.033
plot(verify_two$change_tfl_cycles, verify_two$change_cycles, xlab = "Mean change in TfL counts (adjusted daily flow)", ylab = "Mean change in DfT counts (AADF)")
text(verify_two$change_tfl_cycles, verify_two$change_cycles, verify_two$years, pos = 1, cex = 0.7)
text(verify_two$change_tfl_cycles, verify_two$change_cycles, verify_two$Borough, pos = 1, cex = 0.7)

