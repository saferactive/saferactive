# Copied and edited from dft-tfl-together.R to create an updated version for the GAM model that includes 2020 count data
# Uses AADF rather than raw DfT counter data, so that it can be based on an equivalent dataset to the national/regional analyses.

library(tidyverse)
library(sf)

# Organise TfL dataset ----------------------------------------------------

# TfL counts
tfl_zero = readRDS("tfl-counts-by-site-2020.Rds")
dim(tfl_zero) #6221 #7093

# add geometry
counter_locations = sf::read_sf("tfl-cycle-counter-locations.geojson")
counter_locations = counter_locations %>% rename(`Site ID` = UnqID)
tfl_zero$Borough = gsub("&", "and", tfl_zero$Borough)
counter_locations$Borough = gsub("&", "and", counter_locations$Borough)

counter_sf = inner_join(counter_locations, tfl_zero)
counter_bng = counter_sf %>%
  st_transform(crs = 27700)

# remove very low counts
tfl_0 = counter_bng %>%
  group_by(`Site ID`) %>%
  filter(adjusted_total == 0)
tfl_nonzero = counter_bng %>%
  filter(! `Site ID` %in% tfl_0$`Site ID`,
         mean_site >= 5.0)
dim(tfl_nonzero) #6181 #5613 #6121 #7057

tfl_nonzero$easting = st_coordinates(tfl_nonzero)[,1]
tfl_nonzero$northing = st_coordinates(tfl_nonzero)[,2]

# remove count points not repeated
tfl_repeats = tfl_nonzero %>%
  st_drop_geometry() %>%
  group_by(`Site ID`) %>%
  tally() %>%
  filter(n > 1)

tfl_nonzero = tfl_nonzero %>%
  filter(`Site ID` %in% tfl_repeats$`Site ID`)
dim(tfl_nonzero) #6173 #5605 #6113 #7057

# calculate sum of counts at each point (helps to avoid giving too much weight to counts missing some years)

tfl_nonzero = tfl_nonzero %>%
  group_by(`Site ID`) %>%
  mutate(sum_cycles = sum(adjusted_total),
         n_years = n()) %>%
  ungroup()

# DfT dataset for the years 2010-2015 -------------------------------------

dft_all = readRDS("traffic_joined.Rds")

dft_all = dft_all %>%
  filter(year %in% 2010:2020)
dim(dft_all) #91359

# get mean count per site
dft_all = dft_all %>%
  group_by(count_point_id) %>%
  mutate(mean_cycles = mean(pedal_cycles),
         sum_cycles = sum(pedal_cycles),
         min_cycles = min(pedal_cycles),
         n_years = n()) %>%
  ungroup() %>%
  mutate(change_cycles = pedal_cycles - min_cycles)

# remove very low counts
dft_0 = dft_all %>%
  group_by(count_point_id) %>%
  filter(pedal_cycles == 0)
dft_nonzero = dft_all %>%
  filter(! count_point_id %in% dft_0$count_point_id,
         mean_cycles >= 5.0)
dim(dft_nonzero) #71327

counts_early_years = dft_nonzero %>%
  filter(year %in% 2010:2015)
dim(counts_early_years) #33896

# recalculate change in cycles
counts_early_years = counts_early_years %>%
  group_by(count_point_id) %>%
  mutate(mean_cycles_early = mean(pedal_cycles),
         sum_cycles_early = sum(pedal_cycles),
         min_cycles_early = min(pedal_cycles),
         n_years = n()) %>%
  ungroup() %>%
  mutate(change_cycles_early = pedal_cycles - min_cycles_early)

# remove count points not repeated in the years 2010
dft_early_years_repeats = counts_early_years %>%
  st_drop_geometry() %>%
  select(count_point_id) %>%
  group_by(count_point_id) %>%
  tally() %>%
  filter(n > 1)

counts_early_years$easting = st_coordinates(counts_early_years)[,1]
counts_early_years$northing = st_coordinates(counts_early_years)[,2]

counts_early_years = counts_early_years %>%
  filter(count_point_id %in% dft_early_years_repeats$count_point_id) %>%
  st_drop_geometry()
dim(counts_early_years) #27219



# DfT dataset for the years 2015-2019 -------------------------------------

dft_late_years = dft_nonzero %>%
  filter(year %in% 2015:2020)
dim(dft_late_years) #42341

# recalculate change in cycles
dft_late_years = dft_late_years %>%
  group_by(count_point_id) %>%
  mutate(mean_cycles_late = mean(pedal_cycles),
         sum_cycles_late = sum(pedal_cycles),
         min_cycles_late = min(pedal_cycles),
         n_years = n()) %>%
  ungroup() %>%
  mutate(change_cycles_late = pedal_cycles - min_cycles_late)

# remove count points not repeated in the years 2015-2019
dft_late_years_repeats = dft_late_years %>%
  st_drop_geometry() %>%
  group_by(count_point_id) %>%
  tally() %>%
  filter(n > 1)

dft_late_years = dft_late_years %>%
  filter(count_point_id %in% dft_late_years_repeats$count_point_id)
dim(dft_late_years) #28691

dft_late_years$easting = st_coordinates(dft_late_years)[,1]
dft_late_years$northing = st_coordinates(dft_late_years)[,2]


# Combine DfT and TfL counts for late years -------------------------------

dft_late_years = dft_late_years %>%
  select(year, name = ctyua19nm, count_point_id, easting, northing, pedal_cycles, change_cycles_late, mean_cycles_late, sum_cycles_late, n_years) %>%
  st_drop_geometry()

tfl_nonzero = tfl_nonzero %>%
  select(year, name = Borough, count_point_id = `Site ID`, easting, northing, pedal_cycles = adjusted_total, change_cycles_late = change_cycles, mean_cycles_late = mean_site, sum_cycles_late = sum_cycles, n_years) %>%
  st_drop_geometry()

dft_late_years$data_source = "DfT"
tfl_nonzero$data_source = "TfL"

dft_late_years$count_point_id = as.character(dft_late_years$count_point_id)
counts_combined = bind_rows(dft_late_years, tfl_nonzero)


# Combine the two time periods into a single dataset ----------------------

# intersect(counts_combined, counts_early_years)
counts_combined = counts_combined %>%
  select(year, name, count_point_id, easting, northing, pedal_cycles, change_cycles = change_cycles_late, mean_cycles = mean_cycles_late, sum_cycles = sum_cycles_late, n_years, data_source)
counts_early_years = counts_early_years %>%
  mutate(data_source = "DfT") %>%
  select(year, name = ctyua19nm, count_point_id, easting, northing, pedal_cycles, change_cycles = change_cycles_early, mean_cycles = mean_cycles_early, sum_cycles = sum_cycles_early, n_years, data_source)
counts_early_years$count_point_id = as.character(counts_early_years$count_point_id)
counts_all_years = bind_rows(counts_early_years, counts_combined)


# Explore data ------------------------------------------------------------

forplot = counts_all_years %>%
  group_by(year) %>%
  summarise(change_cycles = weighted.mean(change_cycles, w = n_years))
plot(change_cycles ~ year, data = forplot)

forplot = counts_all_years %>%
  group_by(year) %>%
  summarise(change_cycles = mean(change_cycles))
plot(change_cycles ~ year, data = forplot)

forplot = counts_all_years %>%
  group_by(year) %>%
  summarise(pedal_cycles = weighted.mean(pedal_cycles, w = n_years))
plot(pedal_cycles ~ year, data = forplot)

forplot = counts_early_years %>%
  group_by(year) %>%
  summarise(change_cycles = weighted.mean(change_cycles, w = sum_cycles))
plot(change_cycles ~ year, data = forplot)

forplot = tfl_nonzero %>%
  group_by(year) %>%
  summarise(change_cycles = mean(pedal_cycles))
plot(change_cycles ~ year, data = forplot)

forplot = tfl_nonzero %>%
  group_by(year) %>%
  summarise(change_cycles = weighted.mean(change_cycles_late, w = n_years))
plot(change_cycles ~ year, data = forplot)

ggplot(forplot) +
  geom_line(aes(year, change_cycles, colour = "darkred")) +
  xlab("Year") +
  ylab("Mean change in peak cycle flows (DfT counts)") +
  theme(legend.position = "none")

counts_combined %>%
  group_by(name, year) %>%
  summarise(change_cycles = weighted.mean(change_cycles, w = sum_cycles)) %>%
  View()

tfl_nonzero %>%
  group_by(name, year) %>%
  summarise(change_cycles = weighted.mean(change_cycles_late, w = sum_cycles_late)) %>%
  View()

forplot = counts_combined %>%
  group_by(year) %>%
  summarise(change_cycles = weighted.mean(change_cycles, w = sum_cycles))
plot(change_cycles ~ year, data = forplot)

forplot = counts_combined %>%
  group_by(year, data_source) %>%
  summarise(change_cycles = weighted.mean(change_cycles, w = sum_cycles_late))
plot(change_cycles ~ year, data = forplot)

ggplot(forplot, aes(x = year, y = change_cycles)) +
  geom_line(aes(color = data_source)) +
  scale_color_manual(values = c("darkred", "steelblue")) +
  xlab("Year") +
  ylab("Mean change in peak cycle flows") +
  labs(colour = "Data source")

# ggplot(forplot) +
#   geom_line(aes(year, change_cycles, colour = data_source))

plot(counts_combined$year, counts_combined$change_cycles)

# dd = tfl_nonzero %>%
#   group_by(count_point_id) %>%
#   tally()
# unique(dd$n)

#investigate response variables
hist(counts_combined$pedal_cycles, breaks = 100)
hist(counts_early_years$pedal_cycles, breaks = 50)


# GAM for all years -------------------------------------------------------


library(mgcv)

M = list(c(1, 0.5), NA)

m = bam(pedal_cycles ~
          s(year, bs = "cr", k = 8)
        + s(easting, northing, k = 200, bs = 'ds', m = c(1, 0.5))
        + ti(easting, northing, year, d = c(2,1), bs = c('ds','cr'), m = M, k = c(100, 6))
        ,
        weights = n_years,
        family = scat,
        data = counts_all_years, method = 'fREML',
        nthreads = 4, discrete = TRUE)

summary(m)
obj = plot(m, pages = 4, scheme = 2, shade = TRUE, seWithMean = TRUE)
obj

saveRDS(obj, "model_plot.Rds")


# assign the framework that will be used as a basis for predictions
pdata = with(counts_all_years,
             expand.grid(year = seq(min(year), max(year), by = 1),
                         easting = seq(0, 656000, by = 500), # changed this to make regular 500m grid squares. these include the tip of cornwall and lowestoft.
                         northing = seq(8000, 1215000, by = 500)))
# make predictions according to the GAM model
fitted = predict(m, newdata = pdata, type = "response", newdata.guaranteed = TRUE) #SLOW
# predictions for points far from any counts set to NA
ind = exclude.too.far(pdata$easting, pdata$northing,
                      counts_all_years$easting, counts_all_years$northing, dist = 0.1)
# dist = 0.02) # for calculation of borough means

fitted[ind] = NA
# join the predictions with the framework data
pred_all_points_year = cbind(pdata, Fitted = fitted)
pred_all_points_year = pred_all_points_year %>%
  drop_na

# saveRDS(pred_all_points_year, "gam-all-year-peak-grid-national.Rds")
# saveRDS(pred_all_points_year, "gam-all-year-grid-national.Rds")
saveRDS(pred_all_points_year, "gam-2020-grid-national2.Rds")

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

lads = readRDS("lads.Rds")

borough_geom = lads %>%
  dplyr::select(Name) %>%
  st_transform(27700)

## Assign to borough for predictions by year
pred_sf = pred_all_points_year %>%
  st_as_sf(coords = c("easting", "northing"), crs = 27700)
point_to_borough = st_join(x = pred_sf, y = borough_geom) #SLOW
## Calculate mean annual predictions for each borough
gam_with_lads = point_to_borough %>%
  drop_na() %>%
  group_by(Borough, year) %>%
  summarise(gam_change_cycles = mean(Fitted))
View(gam_with_lads)

# saveRDS(gam_all_year, "gam-all-year.Rds")
saveRDS(gam_with_lads, "gam-national-with-lads-peak.Rds")
saveRDS(gam_with_lads, "gam-national-with-lads.Rds")


# Adjustment factors for national grid --------------------------

# gam_all_year = readRDS("gam-all-year-peak-grid-national.Rds")
gam_all_year = readRDS("gam-2020-grid-national2.Rds")

# get change relative to 2011 for the early years
gam_2011 = gam_all_year %>%
  filter(year == 2011) %>%
  ungroup() %>%
  mutate(change_2011 = Fitted) %>%
  select(easting, northing, change_2011)

gam_all_results = inner_join(gam_all_year, gam_2011) %>%
  mutate(change_relative_to_2011 = Fitted / change_2011)


# collate full results
gam_full_results = gam_all_results %>%
  select(easting, northing, year, change_cycles = change_relative_to_2011)
dim(gam_full_results)
View(gam_full_results)

forplot = gam_full_results %>% group_by(year) %>%
  summarise(change_cycles = mean(change_cycles))
ggplot(forplot) +
  geom_line(aes(year, change_cycles)) +
  xlab("Year") +
  ylab("Mean predicted cycle count (relative to 2011 value)")

# saveRDS(gam_full_results, "gam-full-results-peak-grid-national.Rds")
# piggyback::pb_upload("gam-full-results-peak-grid-national.Rds")

saveRDS(gam_full_results, "gam-full-results-grid-national-2020-2.Rds")
# piggyback::pb_upload("gam-full-results-grid-national.Rds", tag = "0.1.4")
piggyback::pb_upload("gam-full-results-grid-national-2020.Rds")

# GAM model for early years  ----------------------------------------------


library(mgcv)

M = list(c(1, 0.5), NA)

m = bam(change_cycles ~
          s(year, bs = "cr", k = 4)
        + s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5))
        + ti(easting, northing, year, d = c(2,1), bs = c('ds','cr'), m = M, k = c(25, 4))
        ,
        weights = n_years,
        family = scat,
        data = counts_early_years, method = 'fREML',
        nthreads = 4, discrete = TRUE)

summary(m)
plot(m, pages = 4, scheme = 2, shade = TRUE)



# assign the framework that will be used as a basis for predictions
pdata = with(counts_early_years,
             expand.grid(year = seq(min(year), max(year), by = 1),
                         easting = seq(round((min(easting)*2), digits = -3)/2, round((max(easting)*2), digits = -3)/2, by = 500), # changed this to make regular 1km grid squares
                         northing = seq(round((min(northing)*2), digits = -3)/2, round((max(northing)*2), digits = -3)/2, by = 500)))
# make predictions according to the GAM model
fitted = predict(m, newdata = pdata, type = "response", newdata.guaranteed = TRUE) #SLOW
# predictions for points far from any counts set to NA
ind = exclude.too.far(pdata$easting, pdata$northing,
                      counts_early_years$easting, counts_early_years$northing, dist = 0.1)
# dist = 0.02) # for calculation of borough means
fitted[ind] = NA
# join the predictions with the framework data
pred_all_points_year = cbind(pdata, Fitted = fitted)
pred_all_points_year = pred_all_points_year %>%
  drop_na

saveRDS(pred_all_points_year, "gam-early-year-peak-grid-national.Rds")

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

lads = spData::lnd %>% rename(Borough = NAME) %>%
  mutate(Borough = as.character(Borough)) %>%
  mutate(Name = abbreviate(Borough, minlength = 2))

borough_geom = lads %>%
  dplyr::select(Borough) %>%
  st_transform(27700)

## Assign to borough for predictions by year
pred_sf = pred_all_points_year %>%
  st_as_sf(coords = c("easting", "northing"), crs = 27700)
point_to_borough = st_join(x = pred_sf, y = borough_geom)
## Calculate mean annual predictions for each borough
gam_early_year = point_to_borough %>%
  drop_na() %>%
  group_by(Borough, year) %>%
  summarise(gam_change_cycles = mean(Fitted))
View(gam_early_year)

# saveRDS(gam_early_year, "gam-early-year.Rds")
saveRDS(gam_early_year, "gam-early-year-peak.Rds")



# GAM model for late years  ----------------------------------------------

M = list(c(1, 0.5), NA)

m2 = bam(change_cycles ~
           s(year, bs = "cr", k = 3)
         + s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5))
         + ti(easting, northing, year, d = c(2,1), bs = c('ds','cr'), m = M, k = c(25, 3))
         ,
         weights = n_years,
         family = scat,
         data = dft_late_years, method = 'fREML',
         nthreads = 4, discrete = TRUE)

summary(m2)
plot(m2, pages = 4, scheme = 2, shade = TRUE)



# assign the framework that will be used as a basis for predictions
pdata = with(counts_combined,
             expand.grid(year = seq(min(year), max(year), by = 1),
                         easting = seq(round((min(easting)*2), digits = -3)/2, round((max(easting)*2), digits = -3)/2, by = 500), # changed this to make regular 1km grid squares
                         northing = seq(round((min(northing)*2), digits = -3)/2, round((max(northing)*2), digits = -3)/2, by = 500)))
# make predictions according to the GAM model
fitted = predict(m2, newdata = pdata, type = "response", newdata.guaranteed = TRUE)
# predictions for points far from any counts set to NA
ind = exclude.too.far(pdata$easting, pdata$northing,
                      counts_combined$easting, counts_combined$northing,
                      dist = 0.1)
# dist = 0.02) # for calculation of borough means
fitted[ind] = NA
# join the predictions with the framework data
pred_all_points_year = cbind(pdata, Fitted = fitted)
pred_all_points_year = pred_all_points_year %>%
  drop_na

saveRDS(pred_all_points_year, "gam-late-year-peak-grid.Rds")

# Get confidence intervals
confint = predict(m2, newdata = pdata, type = "link")

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
gam_late_year = point_to_borough %>%
  drop_na() %>%
  group_by(Borough, year) %>%
  summarise(gam_change_cycles_late = mean(Fitted))
View(gam_late_year)

# saveRDS(gam_late_year, "gam_late_year.Rds")
saveRDS(gam_late_year, "gam-late-year-peak.Rds")


# Compare TfL counts with joint GAM predictions -----------------------------------------

# counter_la_results = read.csv("tfl-counter-results-london-boroughs-2015-2019.csv")
counter_la_results = read.csv("tfl-counter-results-london-boroughs-2015-2019-peak.csv")
gam_late_year = readRDS("gam-late-year-peak.Rds")

verify = left_join(counter_la_results, gam_late_year, by = c("year", "Borough"))

cor(verify$change_tfl_cycles, verify$gam_change_cycles_late)^2 #R squared = 0.15 #peak 0.194 #peak 0.166
plot(x = verify$change_tfl_cycles, y = verify$gam_change_cycles_late, xlab = "TfL change in mean cycle counts (seasonally adjusted peak hour flow)", ylab = "GAM predictions of change in cycling uptake")
# line(x = verify$change_tfl_cycles, y = verify$gam_change_cycles_late)
# ggsave(plot = tosave, "figures/gam-change-late.png")


# Adjustment factors from 2011, for boroughs--------------------------------

# gam_early_year = readRDS("gam-early-year.Rds")
gam_early_year = readRDS("gam-early-year-peak.Rds")

# get change relative to 2011 for the early years
gam_2011 = gam_early_year %>%
  st_drop_geometry() %>%
  filter(year == 2011) %>%
  ungroup() %>%
  mutate(change_2011 = gam_change_cycles) %>%
  select(Borough, change_2011)

gam_early_results = inner_join(gam_early_year, gam_2011) %>%
  mutate(change_relative_to_2011 = gam_change_cycles / change_2011)

# bridge to the late years
gam_up_to_2015 = gam_early_results %>%
  filter(year == 2015) %>%
  st_drop_geometry() %>%
  select(Borough, change_relative_to_2011)

gam_2015 = gam_late_year %>%
  st_drop_geometry() %>%
  filter(year == 2015) %>%
  ungroup() %>%
  mutate(change_2015 = gam_change_cycles_late) %>%
  select(Borough, change_2015)

gam_2015_onwards = inner_join(gam_late_year, gam_2015) %>%
  mutate(change_relative_to_2015 = gam_change_cycles_late / change_2015)

gam_bridged = inner_join(gam_2015_onwards, gam_up_to_2015, by = "Borough")

gam_bridged = gam_bridged %>%
  mutate(complete_change = change_relative_to_2011*change_relative_to_2015)

# collate full results
gam_bridged %>% select(Borough, year, change_cycles = complete_change)
gam_early_results %>% select(Borough, year, change_cycles = change_relative_to_2011)

gam_full_results = rbind(
  (gam_early_results %>% select(Borough, year, change_cycles = change_relative_to_2011)),
  (gam_bridged %>% select(Borough, year, change_cycles = complete_change) %>%
     filter(year != 2015))
) %>%
  st_drop_geometry()
dim(gam_full_results)
View(gam_full_results)

ggplot(gam_full_results) +
  geom_line(aes(year, change_cycles, colour = Borough))

saveRDS(gam_full_results, "gam-full-results-peak.Rds")

lads_data = inner_join(lads, gam_full_results)

library(tmap)
tm_shape(lads_data) +
  tm_polygons("change_cycles", palette = "BrBG", n = 6) +
  tm_text(text = "Name", size = 0.7) +
  tm_facets("year")


# Adjustment factors from 2011, for spatial grid --------------------------

gam_early_year = readRDS("gam-early-year-peak-grid.Rds")
gam_late_year = readRDS("gam-late-year-peak-grid.Rds")

# get change relative to 2011 for the early years
gam_2011 = gam_early_year %>%
  filter(year == 2011) %>%
  ungroup() %>%
  mutate(change_2011 = Fitted) %>%
  select(easting, northing, change_2011)

gam_early_results = inner_join(gam_early_year, gam_2011) %>%
  mutate(change_relative_to_2011 = Fitted / change_2011)

# bridge to the late years
gam_up_to_2015 = gam_early_results %>%
  filter(year == 2015) %>%
  select(easting, northing, change_relative_to_2011)

gam_2015 = gam_late_year %>%
  filter(year == 2015) %>%
  ungroup() %>%
  mutate(change_2015 = Fitted) %>%
  select(easting, northing, change_2015)

gam_2015_onwards = inner_join(gam_late_year, gam_2015) %>%
  mutate(change_relative_to_2015 = Fitted / change_2015)

gam_bridged = inner_join(gam_2015_onwards, gam_up_to_2015, by = c("easting", "northing"))

gam_bridged = gam_bridged %>%
  mutate(complete_change = change_relative_to_2011*change_relative_to_2015)

# collate full results
gam_bridged %>% select(easting, northing, year, change_cycles = complete_change)
gam_early_results %>% select(easting, northing, year, change_cycles = change_relative_to_2011)

gam_full_results = rbind(
  (gam_early_results %>% select(easting, northing, year, change_cycles = change_relative_to_2011)),
  (gam_bridged %>% select(easting, northing, year, change_cycles = complete_change) %>%
     filter(year != 2015))
)
dim(gam_full_results)
View(gam_full_results)

forplot = gam_full_results %>% group_by(year) %>%
  summarise(change_cycles = mean(change_cycles))
ggplot(forplot) +
  geom_line(aes(year, change_cycles)) +
  xlab("Year") +
  ylab("Mean change in predicted cycle count for London grid cells")

saveRDS(gam_full_results, "gam-full-results-peak-grid.Rds")




