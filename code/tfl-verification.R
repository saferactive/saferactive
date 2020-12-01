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

counter_peak = counter_df %>%
  filter(Period == "AM peak (07:00-10:00)" | Period == "PM peak (16:00-19:00)")
unique(counter_peak$Period)


#find year
counter_peak$year = case_when(is.na(counter_peak$Survey_wave) ~ lubridate::year(counter_peak$`Survey date`), TRUE ~ as.numeric(substr(counter_peak$Survey_wave, 1, 4)))
counter_peak = counter_peak %>%
  filter(! is.na(year))
dim(counter_peak) #945728 #472856 peak


# Select the correct survey wave only

#create ID variable
counter_peak$id = rownames(counter_peak)

# remove rows where survey wave doesn't match survey date
remove1 = counter_peak %>%
      filter(substr(counter_peak$Survey_wave,6,7) == "Q1" & !((substr(counter_peak$`Survey date`,6,7) == "01") | (substr(counter_peak$`Survey date`,6,7) == "02") | (substr(counter_peak$`Survey date`,6,7) == "03")))

counter_peak = counter_peak %>%
  filter(! id %in% remove1$id)

remove2 = counter_peak %>%
      filter(substr(counter_peak$Survey_wave,6,7) == "Q2" & !((substr(counter_peak$`Survey date`,6,7) == "04") | (substr(counter_peak$`Survey date`,6,7) == "05") | (substr(counter_peak$`Survey date`,6,7) == "06")))

counter_peak = counter_peak %>%
  filter(! id %in% remove2$id)

remove3 = counter_peak %>%
      filter(substr(counter_peak$Survey_wave,6,7) == "Q3" & !((substr(counter_peak$`Survey date`,6,7) == "07") | (substr(counter_peak$`Survey date`,6,7) == "08") | (substr(counter_peak$`Survey date`,6,7) == "09")))

counter_peak = counter_peak %>%
  filter(! id %in% remove3$id)

remove4 = counter_peak %>%
      filter(substr(counter_peak$Survey_wave,6,7) == "Q4" & !((substr(counter_peak$`Survey date`,6,7) == "10") | (substr(counter_peak$`Survey date`,6,7) == "11") | (substr(counter_peak$`Survey date`,6,7) == "12")))

counter_peak = counter_peak %>%
  filter(! id %in% remove4$id)

dim(counter_peak) #929008 #464694 peak

############# Weather
doubles = counter_peak %>%
  group_by(`Site ID`, Direction, year, Time, Location, Survey_wave) %>%
  summarise(n_repeats = n()) %>%
  ungroup()

weather_repeats = doubles %>%
  filter(n_repeats == 2)

remove5 = weather_repeats %>%
  inner_join(counter_peak, by = c("Site ID" = "Site ID", "Direction" = "Direction", "year" = "year", "Time" = "Time", "Location" = "Location", "Survey_wave" = "Survey_wave")) %>%
    filter(Weather == "Dry")

counter_peak = counter_peak %>%
  filter(! id %in% remove5$id)
dim(counter_peak) #928816 #464598 peak

###################

# group by survey wave, site and direction
# select only sites which have a full 12 hours (48 periods) of survey data (this can be across several survey dates but must be within the same survey wave/quarter)
counter_daily = counter_peak %>%
  group_by(`Site ID`, Direction, year, Survey_wave) %>%
  summarise(
    n_periods = n(),
    total_daily = sum(`Total cycles`, na.rm = TRUE),
    mean_daily = mean(`Total cycles`, na.rm = TRUE)) %>%
  ungroup()

counter_clean = left_join(counter_peak, counter_daily, by = c("Site ID", "Direction", "year", "Survey_wave")) %>%
  # filter(n_periods == 48) #for whole day
  filter(n_periods == 24) #for peak hours
dim(counter_clean) #919440 #460824

# CENCY201 Tooley Street Survey wave changes while Survey date stays the same
# not enough hours only covers 7am - 2pm

# View(counter_clean %>%
#   group_by(n_periods) %>%
#   tally())

# unique(counter_clean$year)
# unique(substr(counter_clean$Survey_wave,6,7))
# unique(substr(counter_clean$`Survey date`,6,7))

# counter_peak %>%
#   filter(substr(counter_peak$Survey_wave, 6, 7) == "Q1")

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
dim(counter_clean) #846528 #424320 peak

# saveRDS(counter_clean, "counter_clean.Rds")
saveRDS(counter_clean, "counter_clean_peak.Rds")

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
dim(counter_days) #17636 #17680 peak

# Use annual equivalent adjustment factors taken from TfL Central London grid survey data to account for seasonal variation
# Also helps to correct central london counts for varying numbers in each survey wave (Corrects for missing survey wave 4 in 2019)
season_adjust = counter_days %>%
  mutate(wave_number = substr(Survey_wave, 6, 7),
         adjusted_total = case_when(wave_number == "Q1" ~ total_daily * 1.14,
                                    wave_number == "Q2" ~ total_daily * 0.93,
                                    wave_number == "Q3" ~ total_daily * 0.91,
                                    wave_number == "Q4" ~ total_daily * 1.06,
                                    TRUE ~ total_daily * 0.93)) # inner and outer London data were surveyed in Q2

# Combine bidirectional counts into single count for each site
check = season_adjust %>% group_by(`Site ID`, Survey_wave, year) %>% tally()
unique(check$n) #2

season_adjust = season_adjust %>%
  group_by(year, Survey_wave, Borough, `Site ID`, ProgID, Location, wave_number) %>%
  summarise(total_daily = sum(total_daily),
            adjusted_total = sum(adjusted_total))
dim(season_adjust) #8818 #8840 peak

# Calculate change in TfL counts ------------------------------------------


counter_change = season_adjust %>%
  group_by(`Site ID`) %>%
  mutate(mean_site = mean(adjusted_total)) %>%
  ungroup() %>%
  filter(mean_site > 0) %>% # remove counts where there has never been a cyclist
  mutate(change_cycles = adjusted_total/mean_site)
sum(is.na(counter_change$change_cycles))/nrow(counter_change) #0


#Find most common weather type through the day
# counter_change = counter_change %>%
#   group_by(year, Survey_wave, `Site ID`, Direction) %>%
#   mutate(main_weather =


dim(counter_change %>% filter(ProgID == "CENCY")) #3609
dim(counter_change %>% filter(ProgID == "INNCY")) #2962
dim(counter_change %>% filter(ProgID == "OUTCY")) #2247

dim(counter_change %>% filter(ProgID == "CENCY") %>% select(`Site ID`) %>% unique()) #209
dim(counter_change %>% filter(ProgID == "INNCY") %>% select(`Site ID`) %>% unique()) #597
dim(counter_change %>% filter(ProgID == "OUTCY") %>% select(`Site ID`) %>% unique()) #451

# Survey wave corrections -------------------------------------------------
# Group by `Site ID` and year so Central London counts that have been surveyed 4 times a year don't have 4 times more influence than Inner/Outer London counts

# get mean values for each site_location and year
counter_year = counter_change %>%
  group_by(year, Borough, `Site ID`, ProgID, Location, mean_site) %>%
  summarise(
    total_daily = mean(total_daily),
    adjusted_total = mean(adjusted_total),
    change_cycles = mean(change_cycles)
  ) %>%
  ungroup()
dim(counter_year) #6221


dim(counter_year %>% filter(ProgID == "CENCY")) #1012
dim(counter_year %>% filter(ProgID == "INNCY")) #2962
dim(counter_year %>% filter(ProgID == "OUTCY")) #2247



saveRDS(counter_year, "tfl-counts-by-site.Rds")
saveRDS(counter_year, "tfl-counts-by-site-peak.Rds")


# Group by borough --------------------------------------------------------


counter_means_year = counter_year %>%
  group_by(year, Borough) %>%
  summarise(
    n_counters = length(unique(`Site ID`)),
    borough_mean = mean(adjusted_total, na.rm = TRUE),
    change_tfl_cycles = weighted.mean(change_cycles, w = mean_site, na.rm = TRUE)
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
readr::write_csv(counter_la_results, "tfl-counter-results-london-boroughs-2015-2019-peak.csv")
piggyback::pb_upload("tfl-counter-results-london-boroughs-2015-2019-peak.csv", repo = "itsleeds/saferroadsmap")

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
  tm_polygons("change_tfl_cycles", palette = "BrBG", n = 6, style = "jenks") +
  tm_text(text = "Name", size = 0.7)

counter_lateyear = lads_data %>%
  filter(year %in% 2017:2019) %>%
  group_by(Borough, Name) %>%
  summarise(borough_mean = mean(borough_mean),
            change_tfl_cycles = mean(change_tfl_cycles)) %>%
  mutate(years = "2017-19")

tm_shape(counter_lateyear) +
  tm_polygons("change_tfl_cycles", palette = "BrBG", n = 6, style = "jenks") +
  tm_text(text = "Name", size = 0.7)

tfl_early_v_late = rbind(counter_earlyyear, counter_lateyear)


# Grouping the boroughs ---------------------------------------------------

# loosely based on London Assembly constituencies

ctn = c("City of London", "Tower Hamlets", "Newham")
ci = c("Camden", "Islington")
wkh = c("Westminster", "Kensington and Chelsea", "Hammersmith and Fulham")
sl = c("Southwark", "Lambeth")
lg = c("Lewisham", "Greenwich")
hw = c("Hackney", "Waltham Forest")
wm = c("Wandsworth", "Merton")
heb = c("Haringey", "Enfield", "Barnet")
hb = c("Harrow", "Brent")
eh = c("Ealing", "Hillingdon")
hrk = c("Hounslow", "Richmond upon Thames", "Kingston upon Thames")
cs = c("Croydon", "Sutton")
bb = c("Bromley", "Bexley")
hbr = c("Havering", "Barking and Dagenham", "Redbridge")
areas = list(ctn, ci, wkh, sl, lg, hw, wm, heb, hb, eh, hrk, cs, bb, hbr)
a2 = c("ctn", "ci", "wkh", "sl", "lg", "hw", "wm", "heb", "hb", "eh", "hrk", "cs", "bb", "hbr")

counter_la_results$area = for(i in 1:length(areas)){
  if(counter_la_results$Borough %in% areas[[i]]) {counter_la_results$area = a2[i]}
}

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
        weights = mean_site,
        family = scat,
        data = counter_bam, method = 'fREML',
        nthreads = 4, discrete = TRUE)

summary(m)
plot(m, pages = 4, scheme = 2, shade = TRUE)



# assign the framework that will be used as a basis for predictions
pdata = with(counter_bam,
             expand.grid(year = seq(min(year), max(year), by = 1),
                         easting = seq(round((min(easting)*2), digits = -3)/2, round((max(easting)*2), digits = -3)/2, by = 500), # changed this to make regular 1km grid squares
                         northing = seq(round((min(northing)*2), digits = -3)/2, round((max(northing)*2), digits = -3)/2, by = 500)))
# make predictions according to the GAM model
fitted = predict(m, newdata = pdata, type = "response", newdata.guaranteed = TRUE)
# predictions for points far from any counts set to NA
ind = exclude.too.far(pdata$easting, pdata$northing,
                      counter_bam$easting, counter_bam$northing, dist = 0.02)
fitted[ind] = NA
# join the predictions with the framework data
pred_all_points_year = cbind(pdata, Fitted = fitted)
pred_all_points_year = pred_all_points_year %>%
  drop_na


ggplot(pred_all_points_year, aes(x = easting, y = northing)) +
  geom_raster(aes(fill = Fitted)) + facet_wrap(~ year, ncol = 5) +
  viridis::scale_fill_viridis(name = "Pedal cycles", option = 'plasma',
                              na.value = 'transparent') +
  coord_fixed(ratio = 1) +
  # geom_line(lads, alpha(0.1)) +
  theme(legend.position = 'top', legend.key.width = unit(2, 'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

borough_geom = lads %>%
  dplyr::select(Borough) %>%
  st_transform(27700)

## Assign to borough for predictions by year
pred_sf = pred_all_points_year %>%
  st_as_sf(coords = c("easting", "northing"), crs = 27700)
point_to_borough = st_join(x = pred_sf, y = borough_geom)
## Calculate mean annual predictions for each borough
tfl_gam_change = point_to_borough %>%
  drop_na() %>%
  group_by(Borough, year) %>%
  summarise(gam_change_cycles = mean(Fitted))
View(tfl_gam_change)

saveRDS(tfl_gam_change, "tfl-gam-change.Rds")
#####################################

m2 = bam(adjusted_total ~
          s(year, k = 5)
        + s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5))
        + ti(easting, northing, year, d = c(2,1), bs = c('ds','tp'), m = M, k = c(25, 3))
        ,
        family = nb(link = "log"),
        data = counter_bam, method = 'fREML',
        nthreads = 4, discrete = TRUE)


summary(m2)
plot(m2, pages = 4, scheme = 2, shade = TRUE)


# assign the framework that will be used as a basis for predictions
pdata = with(counter_bam,
             expand.grid(year = seq(min(year), max(year), by = 1),
                         easting = seq(round((min(easting)*2), digits = -3)/2, round((max(easting)*2), digits = -3)/2, by = 500), # changed this to make regular 1km grid squares
                         northing = seq(round((min(northing)*2), digits = -3)/2, round((max(northing)*2), digits = -3)/2, by = 500)))
# make predictions according to the GAM model
fitted = predict(m2, newdata = pdata, type = "response", newdata.guaranteed = TRUE)
# predictions for points far from any counts set to NA
ind = exclude.too.far(pdata$easting, pdata$northing,
                      counter_bam$easting, counter_bam$northing, dist = 0.02)
fitted[ind] = NA
# join the predictions with the framework data
pred_all_points_year = cbind(pdata, Fitted = fitted)
pred_all_points_year = pred_all_points_year %>%
  drop_na


ggplot(pred_all_points_year, aes(x = easting, y = northing)) +
  geom_raster(aes(fill = Fitted)) + facet_wrap(~ year, ncol = 5) +
  viridis::scale_fill_viridis(name = "Pedal cycles", option = 'plasma',
                     na.value = 'transparent') +
  coord_fixed(ratio = 1) +
  # geom_line(lads, alpha(0.1)) +
  theme(legend.position = 'top', legend.key.width = unit(2, 'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

borough_geom = lads %>%
  dplyr::select(Borough) %>%
  st_transform(27700)

## Assign to borough for predictions by year
pred_sf = pred_all_points_year %>%
  st_as_sf(coords = c("easting", "northing"), crs = 27700)
point_to_borough = st_join(x = pred_sf, y = borough_geom)
## Calculate mean annual predictions for each borough
tfl_gam_preds = point_to_borough %>%
  drop_na() %>%
  group_by(Borough, year) %>%
  summarise(borough_mean_cycles = mean(Fitted))
View(tfl_gam_preds)

saveRDS(tfl_gam_preds, "tfl-gam-preds.Rds")

# Compare TfL counts with DfT GAM predictions -----------------------------------------

counter_la_results = read.csv("tfl-counter-results-london-boroughs-2015-2019.csv")

# gam_preds = readRDS("pred-borough-change-cycles.Rds")
dft_gam_preds = readRDS("london-dft-gam-preds-with-esti.Rds")

verify = left_join(counter_la_results, dft_gam_preds, by = c("year", "Borough" = "Name"))

cor(verify$change_tfl_cycles, verify$borough_change_cycles)^2 #R squared = 0.00052
plot(x = verify$change_tfl_cycles, y = verify$borough_change_cycles, xlab = "TfL change in cycle counts (adjusted daily flow)", ylab = "GAM predictions of change (AADF)")
# ggsave(plot = tosave, "figures/gam-change-comparison.png")

verify = verify %>%
  filter(year != 2015)
dim(verify)
cor(verify$count_relative_to_2015, verify$pred_relative_to_2015)^2 #R squared = 0.0019
plot(x = verify$count_relative_to_2015, y = verify$pred_relative_to_2015, xlab = "TfL counts relative to 2015", ylab = "GAM predictions of change relative to 2015")
# ggsave(plot = tosave, "figures/change-relative-to-2015.png")


# Correlation between TfL and DfT counts with estimated DfT counts (by borough) ---------------------

piggyback::pb_download("dft-counts-by-borough-with-esti.Rds")
dft_counts_by_borough = readRDS("dft-counts-by-borough-with-esti.Rds")

verify_raw = left_join(counter_la_results, dft_counts_by_borough, by = c("year", "Borough" = "name"))
dim(verify_raw)

# Absolute counts
cor(verify_raw$borough_mean, verify_raw$pedal_cycles)^2 #R squared = 0.949
# verify_raw = verify_raw %>% filter(Borough == "Westminster")
plot(verify_raw$borough_mean, verify_raw$pedal_cycles, xlab = "Mean TfL counts (flow per 15 minutes)", ylab = "Mean AADF from DfT counts (with estimated counts)")
text(verify_raw$borough_mean, verify_raw$pedal_cycles, verify_raw$year, pos = 1, cex = 0.7)
text(verify_raw$borough_mean, verify_raw$pedal_cycles, verify_raw$Borough, pos = 1, cex = 0.7)

# Raw change
cor(verify_raw$change_tfl_cycles, verify_raw$change_cycles)^2 #R squared = 0.0002
plot(verify_raw$change_tfl_cycles, verify_raw$change_cycles, xlab = "Mean change in TfL counts (adjusted daily flow)", ylab = "Mean change in DfT counts (AADF)")
text(verify_raw$change_tfl_cycles, verify_raw$change_cycles, verify_raw$year, pos = 1, cex = 0.7)
text(verify_raw$change_tfl_cycles, verify_raw$change_cycles, verify_raw$Borough, pos = 1, cex = 0.7)

# Absolute GAM predictions
dft_gam_preds = readRDS("london-dft-gam-preds-with-esti.Rds")

verify_gam = left_join(
  (tfl_gam_preds %>% st_drop_geometry() %>% rename(tfl_mean_cycles = borough_mean_cycles)),
  dft_gam_preds,  by = c("year", "Borough" = "Name"))
cor(verify_gam$tfl_mean_cycles, verify_gam$borough_mean_cycles)^2 #R squared = 0.945
plot(verify_gam$tfl_mean_cycles, verify_gam$borough_mean_cycles, xlab = "Mean cycles (GAM prediction using TfL counts)", ylab = "Mean cycels (GAM prediction using DfT counts)")
text(verify_gam$tfl_mean_cycles, verify_gam$borough_mean_cycles, verify_gam$year, pos = 1, cex = 0.7)

# GAM change predictions (borough level)
dft_gam_change = readRDS("pred-borough-change-cycles.Rds")


verify_gam = left_join(
  (tfl_gam_change %>% st_drop_geometry()),
  dft_gam_change,  by = c("year", "Borough"))
cor(verify_gam$gam_change_cycles, verify_gam$borough_change_cycles)^2 #R squared = 0.042
plot(verify_gam$gam_change_cycles, verify_gam$borough_change_cycles, xlab = "Mean change in cycles (GAM prediction using TfL counts)", ylab = "Mean change in cycles (GAM prediction using DfT counts)")
text(verify_gam$gam_change_cycles, verify_gam$borough_change_cycles, verify_gam$year, pos = 1, cex = 0.7)
text(verify_gam$gam_change_cycles, verify_gam$borough_change_cycles, verify_gam$Borough, pos = 1, cex = 0.7)

# Verify yearly trends ---------------------------------------------------

# Using mean of all count points
traffic_london = readRDS("dft-count-points-with-esti.Rds")

verify_year = left_join(
  (counter_year %>% group_by(year) %>% summarise(adjusted_total = mean(adjusted_total))),
  (traffic_london %>% group_by(year) %>% summarise(pedal_cycles = mean(pedal_cycles))),
  by = "year")
cor(verify_year$adjusted_total, verify_year$pedal_cycles)^2 #R squared = 0.289 # R is negative
plot(verify_year$adjusted_total, verify_year$pedal_cycles, xlab = "Mean change in TfL counts (adjusted daily flow)", ylab = "Mean change in DfT counts (AADF)")
text(verify_year$adjusted_total, verify_year$pedal_cycles, verify_year$year, pos = 1, cex = 0.7)

# Using mean of borough means for count points
verify_year = left_join(
  (counter_la_results %>% group_by(year) %>% summarise(borough_mean = mean(borough_mean))),
  (dft_counts_by_borough %>% group_by(year) %>% summarise(pedal_cycles = mean(pedal_cycles))),
   by = "year")
cor(verify_year$borough_mean, verify_year$pedal_cycles)^2 #R squared = 0.094
plot(verify_year$borough_mean, verify_year$pedal_cycles, xlab = "Mean change in TfL counts (adjusted daily flow)", ylab = "Mean change in DfT counts (AADF)")
text(verify_year$borough_mean, verify_year$pedal_cycles, verify_year$year, pos = 1, cex = 0.7)

# Using GAM count predictions
dft_gam_preds = readRDS("london-dft-gam-preds-with-esti.Rds")

verify_year = left_join(
  (tfl_gam_preds %>% st_drop_geometry() %>% group_by(year) %>% summarise(tfl_mean_cycles = mean(borough_mean_cycles))),
  (dft_gam_preds %>% group_by(year) %>% summarise(borough_mean_cycles = mean(borough_mean_cycles))),
  by = "year")
cor(verify_year$tfl_mean_cycles, verify_year$borough_mean_cycles)^2 #R squared = 0.121 # R is negative
plot(verify_year$tfl_mean_cycles, verify_year$borough_mean_cycles, xlab = "Mean cycles (GAM prediction using TfL counts)", ylab = "Mean cycels (GAM prediction using DfT counts)")
text(verify_year$tfl_mean_cycles, verify_year$borough_mean_cycles, verify_year$year, pos = 1, cex = 0.7)

# Using GAM change predictions
dft_gam_change = readRDS("pred-borough-change-cycles.Rds")

verify_year = left_join(
  (tfl_gam_change %>% st_drop_geometry() %>% group_by(year) %>% summarise(tfl_change_cycles = mean(gam_change_cycles))),
  (dft_gam_change %>% group_by(year) %>% summarise(dft_change_cycles = mean(borough_change_cycles))),
  by = "year")
cor(verify_year$tfl_change_cycles, verify_year$dft_change_cycles)^2 #R squared = 0.208
plot(verify_year$tfl_change_cycles, verify_year$dft_change_cycles, xlab = "Mean cycles (GAM prediction using TfL counts)", ylab = "Mean cycels (GAM prediction using DfT counts)")
text(verify_year$tfl_change_cycles, verify_year$dft_change_cycles, verify_year$year, pos = 1, cex = 0.7)

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
cor(verify_two$borough_mean, verify_two$pedal_cycles)^2 #R squared = 0.957
plot(verify_two$borough_mean, verify_two$pedal_cycles, xlab = "Mean TfL counts (adjusted daily flow)", ylab = "Mean AADF from DfT counts",
      # xlim = c(0, 500), ylim = c(0, 500)
     )
title ("Mean cycle counts per borough over two time periods (2015-16 and 2017-19)", cex.main = 0.8)
text(verify_two$borough_mean, verify_two$pedal_cycles, verify_two$years, pos = 1, cex = 0.7)
text(verify_two$borough_mean, verify_two$pedal_cycles, verify_two$Borough, pos = 1, cex = 0.7)

# Raw change
cor(verify_two$change_tfl_cycles, verify_two$change_cycles)^2 #R squared = 0.020
plot(verify_two$change_tfl_cycles, verify_two$change_cycles, xlab = "Mean change in TfL counts (adjusted daily flow)", ylab = "Mean change in DfT counts (AADF)")
text(verify_two$change_tfl_cycles, verify_two$change_cycles, verify_two$years, pos = 1, cex = 0.7)
text(verify_two$change_tfl_cycles, verify_two$change_cycles, verify_two$Borough, pos = 1, cex = 0.7)

