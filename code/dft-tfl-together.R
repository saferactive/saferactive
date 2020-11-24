
# Organise TfL dataset ----------------------------------------------------

# TfL counts
tfl_zero = readRDS("tfl-counts-by-site.Rds")
dim(tfl_zero) #6221

# add geometry
counter_locations = sf::read_sf("tfl-cycle-counter-locations.geojson")
counter_locations = counter_locations %>% rename(`Site ID` = UnqID)
tfl_zero$Borough = gsub("&", "and", tfl_zero$Borough)

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
dim(tfl_nonzero) #6181

tfl_nonzero$easting = st_coordinates(tfl_nonzero)[,1]
tfl_nonzero$northing = st_coordinates(tfl_nonzero)[,2]

# remove count points not repeated
tfl_repeats = tfl_nonzero %>%
  group_by(`Site ID`) %>%
  tally() %>%
  filter(n > 1)

tfl_nonzero = tfl_nonzero %>%
  filter(`Site ID` %in% tfl_repeats$`Site ID`)
dim(tfl_nonzero) #6173

# calculate sum of counts at each point (helps to avoid giving too much weight to counts missing some years)

tfl_nonzero = tfl_nonzero %>%
  group_by(`Site ID`) %>%
  mutate(sum_cycles = sum(adjusted_total)) %>%
  ungroup()

# DfT dataset for the years 2010-2015 -------------------------------------

# DfT counts
dft_nonzero = readRDS("dft-london-no-zeroes-no-esti.Rds")
dim(dft_nonzero) #5215

counts_early_years = dft_nonzero %>%
  filter(year %in% 2010:2015)
dim(counts_early_years) #2981

# recalculate change in cycles
counts_early_years = counts_early_years %>%
  group_by(count_point_id) %>%
  mutate(mean_cycles_early = mean(pedal_cycles),
         sum_cycles_early = sum(pedal_cycles)) %>%
  ungroup() %>%
  mutate(change_cycles_early = pedal_cycles/mean_cycles_early)

# remove count points not repeated in the years 2010-2015
dft_early_years_repeats = counts_early_years %>%
  group_by(count_point_id) %>%
  tally() %>%
  filter(n > 1)

counts_early_years = counts_early_years %>%
  filter(count_point_id %in% dft_early_years_repeats$count_point_id)
dim(counts_early_years) #2432


# DfT dataset for the years 2015-2019 -------------------------------------

dft_late_years = dft_nonzero %>%
  filter(year %in% 2015:2019)
dim(dft_late_years) #2674

# recalculate change in cycles
dft_late_years = dft_late_years %>%
  group_by(count_point_id) %>%
  mutate(mean_cycles_late = mean(pedal_cycles),
         sum_cycles_late = sum(pedal_cycles)) %>%
  ungroup() %>%
  mutate(change_cycles_late = pedal_cycles/mean_cycles_late)

# remove count points not repeated in the years 2015-2019
dft_late_years_repeats = dft_late_years %>%
  group_by(count_point_id) %>%
  tally() %>%
  filter(n > 1)

dft_late_years = dft_late_years %>%
  filter(count_point_id %in% dft_late_years_repeats$count_point_id)
dim(dft_late_years) #2021


# Annual change comparisons
# dft_nonzero %>%
#   group_by(year) %>%
#   summarise(change_cycles = mean(change_cycles))
#
# dft_nonzero %>% filter(year > 2014) %>% summarise(change_cycles = mean(change_cycles)) # 1.13
#
# tfl_nonzero %>%
#   group_by(year) %>%
#   summarise(change_cycles = mean(change_cycles))

# # Multiply all TfL change values by 1.13, to correct for the lack of data from earlier years
# tfl_nonzero$change_corrected = tfl_nonzero$change_cycles*1.13

# # Combine the two datasets
# dft_nonzero = dft_nonzero %>%
#   select(year, name, count_point_id, easting, northing, pedal_cycles, change_cycles, mean_cycles)
# tfl_nonzero = tfl_nonzero %>%
#   select(year, name = Borough, count_point_id = `Site ID`, easting, northing, pedal_cycles = adjusted_total, change_cycles = change_corrected, mean_cycles = mean_site) %>%
#   st_drop_geometry()
#
# dft_nonzero$data_source = "DfT"
# tfl_nonzero$data_source = "TfL"
#
# counts_combined = rbind(dft_nonzero, tfl_nonzero)

# Combine DfT and TfL counts for late years -------------------------------

dft_late_years = dft_late_years %>%
  select(year, name, count_point_id, easting, northing, pedal_cycles, change_cycles_late, mean_cycles_late, sum_cycles_late)

tfl_nonzero = tfl_nonzero %>%
    select(year, name = Borough, count_point_id = `Site ID`, easting, northing, pedal_cycles = adjusted_total, change_cycles_late = change_cycles, mean_cycles_late = mean_site, sum_cycles_late = sum_cycles) %>%
    st_drop_geometry()

dft_late_years$data_source = "DfT"
tfl_nonzero$data_source = "TfL"

counts_combined = rbind(dft_late_years, tfl_nonzero)

# Explore data

forplot = counts_early_years %>%
  group_by(year) %>%
  summarise(change_cycles_early = weighted.mean(change_cycles_early, w = sum_cycles_early))
plot(change_cycles_early ~ year, data = forplot)

counts_combined %>%
  group_by(year, name) %>%
  summarise(change_cycles_late = weighted.mean(change_cycles_late, w = sum_cycles_late)) %>%
  View()

forplot = counts_combined %>%
  group_by(year) %>%
  summarise(change_cycles_late = weighted.mean(change_cycles_late, w = sum_cycles_late))
plot(change_cycles_late ~ year, data = forplot)

forplot = counts_combined %>%
  group_by(year, data_source) %>%
  summarise(change_cycles_late = weighted.mean(change_cycles_late, w = sum_cycles_late))
plot(change_cycles_late ~ year, data = forplot)

plot(counts_combined$year, counts_combined$change_cycles_late)

# dd = tfl_nonzero %>%
#   group_by(count_point_id) %>%
#   tally()
# unique(dd$n)


# GAM model for early years  ----------------------------------------------


library(mgcv)

M = list(c(1, 0.5), NA)

m = bam(change_cycles_early ~
          s(year, bs = "cr", k = 4)
        + s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5))
        + ti(easting, northing, year, d = c(2,1), bs = c('ds','cr'), m = M, k = c(25, 3))
        ,
        weights = sum_cycles_early,
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
fitted = predict(m, newdata = pdata, type = "response", newdata.guaranteed = TRUE)
# predictions for points far from any counts set to NA
ind = exclude.too.far(pdata$easting, pdata$northing,
                      counts_early_years$easting, counts_early_years$northing, dist = 0.02)
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
gam_early_year = point_to_borough %>%
  drop_na() %>%
  group_by(Borough, year) %>%
  summarise(gam_change_cycles = mean(Fitted))
View(gam_early_year)

saveRDS(gam_early_year, "gam-early-year")




# GAM model for late years  ----------------------------------------------

M = list(c(1, 0.5), NA)

m2 = bam(change_cycles_late ~
          s(year, bs = "cr", k = 3)
        + s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5))
        + ti(easting, northing, year, d = c(2,1), bs = c('ds','cr'), m = M, k = c(25, 3))
        ,
        weights = sum_cycles_late,
        family = scat,
        data = counts_combined, method = 'fREML',
        nthreads = 4, discrete = TRUE)

summary(m)
plot(m, pages = 4, scheme = 2, shade = TRUE)



# assign the framework that will be used as a basis for predictions
pdata = with(counts_combined,
             expand.grid(year = seq(min(year), max(year), by = 1),
                         easting = seq(round((min(easting)*2), digits = -3)/2, round((max(easting)*2), digits = -3)/2, by = 500), # changed this to make regular 1km grid squares
                         northing = seq(round((min(northing)*2), digits = -3)/2, round((max(northing)*2), digits = -3)/2, by = 500)))
# make predictions according to the GAM model
fitted = predict(m2, newdata = pdata, type = "response", newdata.guaranteed = TRUE)
# predictions for points far from any counts set to NA
ind = exclude.too.far(pdata$easting, pdata$northing,
                      counts_combined$easting, counts_combined$northing, dist = 0.02)
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
gam_late_year = point_to_borough %>%
  drop_na() %>%
  group_by(Borough, year) %>%
  summarise(gam_change_cycles_late = mean(Fitted))
View(gam_late_year)

saveRDS(gam_late_year, "gam_late_year.Rds")





