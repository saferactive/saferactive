library(tidyverse)
library(sf)
library(mapview)

# Input data --------------------------------------------------------------

# Simplified English regions
# piggyback::pb_download("regions_renamed.Rds")
regions = readRDS("regions_renamed.Rds") %>%
  st_transform(27700)

# Full resolution countries and English regions
# BFC December 2020 full resolution clipped English regions. From https://geoportal.statistics.gov.uk/datasets/ons::regions-december-2020-en-bfc/about
# BFC December 2020 full resolution clipped countries https://geoportal.statistics.gov.uk/datasets/ons::countries-december-2020-uk-bfc/about

regions_bfc = read_sf("Regions_.December_2020._EN_BFC.geojson") %>%
  st_transform(27700) %>%
  mutate(region = RGN20NM)

# DfT AADF traffic count data 2000-2020
# piggyback::pb_download("traffic_joined.Rds")
dft_counts = readRDS("traffic_joined.Rds")

# NTS travel data 2003-2019
nts_clean = read.csv("d_region_clean.csv")

# Stats19 collision data 2010-2019
collision_data = readRDS("data/crash_2010_2019_with_summary_adjusted_casualties.Rds") %>%
  mutate(ksi_cycle = casualty_serious_cyclist + casualty_fatal_cyclist) %>%
  st_transform(27700)
collision_data$year = lubridate::year(collision_data$date)

# GAM model raw results for changes in traffic counts 2010-2019
# piggyback::pb_download("gam-full-results-grid-national.Rds")
gam_results = readRDS("gam-full-results-grid-national.Rds")

# # GAM results aggregated by lower tier LA
# gam_by_la = read.csv("la_lower_km_cycled_2010_2019.csv")

# Regional population time series
# piggyback::pb_download("region-populations.Rds")
region_populations = readRDS("region-populations.Rds")


# Single dataset trends ---------------------------------------------------


# DfT AADF counts ---------------------------------------------------------

# Get more reliable DfT data

# points measured at least 5 times 2010-2019
dft_5yr = dft_counts %>%
  filter(year %in% 2010:2020) %>%
  group_by(count_point_id) %>%
  mutate(n = n()) %>%
  filter(n > 4)
length(unique(dft_5yr$count_point_id)) # 6015

# points measured every single year 2010-2019
dft_10yr = dft_counts %>%
  filter(year %in% 2010:2020) %>%
  group_by(count_point_id) %>%
  mutate(n = n()) %>%
  filter(n > 9)
length(unique(dft_10yr$count_point_id)) # 3113

# National DfT data

dft_national_5yr = dft_counts %>%
  filter(count_point_id %in% dft_5yr$count_point_id) %>%
  group_by(year) %>%
  summarise(dft_cycles = mean(pedal_cycles))

# dft_national_10yr = dft_counts %>%
#   filter(count_point_id %in% dft_10yr$count_point_id) %>%
#   group_by(year) %>%
#   summarise(dft_cycles = mean(pedal_cycles))
#
# dft_national = dft_counts %>%
#   group_by(year) %>%
#   summarise(dft_cycles = mean(pedal_cycles))

# Plot national data

dft_national_5yr %>%
  ggplot() +
  geom_line(aes(year, dft_cycles)) +
  # geom_smooth(aes(year, dft_cycles)) + # trend line looks silly with 2020 data
  ylab("Mean cycle AADF")

# dft_national %>%
#   ggplot() +
#   geom_line(aes(year, dft_cycles)) +
#   geom_smooth(aes(year, dft_cycles)) +
#   ylab("Mean cycle AADF")

# Regional DfT data

dft_regional = st_join(dft_counts, regions_bfc)
saveRDS(dft_regional, "dft_regional.Rds")

dft_regional_5yr = dft_regional %>%
  st_drop_geometry() %>%
  filter(count_point_id %in% dft_5yr$count_point_id) %>%
  group_by(RGN20CD, region, year) %>%
  summarise(dft_cycles = mean(pedal_cycles))
summary(dft_regional)

dft_regional_10yr = dft_regional %>%
  st_drop_geometry() %>%
  filter(count_point_id %in% dft_10yr$count_point_id) %>%
  group_by(RGN20CD, region, year) %>%
  summarise(dft_cycles = mean(pedal_cycles))
summary(dft_regional)

dft_regional_all = dft_regional %>%
  st_drop_geometry() %>%
  group_by(RGN20CD, region, year) %>%
  summarise(dft_cycles = mean(pedal_cycles))
summary(dft_regional_all)

# Plot regional data

dft_regional_5yr %>%
  filter(region != "London") %>%
  ggplot() +
  geom_line(aes(year, dft_cycles, colour = region)) +
  # geom_smooth(aes(year, dft_cycles)) +
  labs(y = "Mean cycle AADF", x  = "Year", colour = "Region")

dft_regional_all %>%
  filter(region != "London") %>%
  ggplot() +
  geom_line(aes(year, dft_cycles, colour = region)) +
  # geom_smooth(aes(year, dft_cycles)) +
  labs(y = "Mean cycle AADF", x  = "Year", colour = "Region")

dft_regional_10yr %>%
  filter(region != "London") %>%
  ggplot() +
  geom_line(aes(year, dft_cycles, colour = region)) +
  # geom_smooth(aes(year, dft_cycles)) +
  labs(y = "Mean cycle AADF", x  = "Year", colour = "Region")

# NTS results -------------------------------------------------------------

# Multiply trips by regional population
nts_pop = inner_join(nts_clean, region_populations, by = c("region" = "RGN21NM"))

nts_weighted = nts_pop %>%
  mutate(dist_cycled_per_yr = case_when(
    year == 2003 ~ bicycle * population_2003,
    year == 2004 ~ bicycle * population_2004,
    year == 2005 ~ bicycle * population_2005,
    year == 2006 ~ bicycle * population_2006,
    year == 2007 ~ bicycle * population_2007,
    year == 2008 ~ bicycle * population_2008,
    year == 2009 ~ bicycle * population_2009,
    year == 2010 ~ bicycle * population_2010,
    year == 2011 ~ bicycle * population_2011,
    year == 2012 ~ bicycle * population_2012,
    year == 2013 ~ bicycle * population_2013,
    year == 2014 ~ bicycle * population_2014,
    year == 2015 ~ bicycle * population_2015,
    year == 2016 ~ bicycle * population_2016,
    year == 2017 ~ bicycle * population_2017,
    year == 2018 ~ bicycle * population_2018,
    year == 2019 ~ bicycle * population_2019
  ))

# convert this to % increase from 2011
#normalise then run lm
nts_national = nts_weighted %>%
  group_by(year) %>%
  summarise(nts_cycles = sum(dist_cycled_per_yr))

nts_national %>%
  ggplot() +
  geom_line(aes(year, nts_cycles)) +
  geom_smooth(aes(year, nts_cycles)) +
  ylab("Total distance cycled/yr (km)")

# regional
nts_regional = nts_weighted %>%
  group_by(region, year) %>%
  summarise(nts_cycles = sum(dist_cycled_per_yr))
summary(nts_regional)

nts_regional %>%
  ggplot() +
  geom_line(aes(year, nts_cycles / 1000000000, colour = region), lwd = 0.8) +
  # geom_smooth(aes(year, nts_cycles)) +
  ylab("Total distance cycled (Bkm)") +
  labs(x = "Year", colour = "Region") +
  scale_color_brewer(type = "qual", palette = 3)


# Collisions --------------------------------------------------------------

stats19_national = collision_data %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarise(ksi_cycle = sum(ksi_cycle))

stats19_national %>%
  ggplot() +
  geom_line(aes(year, ksi_cycle)) +
  geom_smooth(aes(year, ksi_cycle)) +
  ylab("Cycle ksi casualties/yr")

# regional
# stats19_regional = st_join(collision_data, regions_bfc)
#
# stats19_regional = stats19_regional %>%
#   st_drop_geometry() %>%
#   group_by(RGN20CD, region, year) %>%
#   summarise(ksi_cycle = sum(ksi_cycle))
# summary(stats19_regional)
#
# saveRDS(stats19_regional, "stats19_regional.Rds")
# piggyback::pb_upload("stats19_regional.Rds")
stats19_regional = readRDS("stats19_regional.Rds")

stats19_regional %>%
  ggplot() +
  geom_line(aes(year, ksi_cycle, colour = region)) +
  # geom_smooth(aes(year, ksi_cycle)) +
  ylab("Sum cycle KSI") +
  labs(x = "Year", colour = "Region")


# GAM results -------------------------------------------------------------

gam_national = gam_results %>%
  st_as_sf(coords = c("easting", "northing"), crs = 27700)

gam_regional = st_join(gam_national, regions) #produces NAs - these are in scotland, wales, the sea, and in england due to the oversimplified regions map

saveRDS(gam_regional, "gam_regional.Rds")

gam_na = gam_regional %>%
  filter(is.na(region))

gam_na_slice = slice_sample(gam_na, prop = 0.001)
mapview(gam_na_slice)

# GAM national
# this doesn't weight for population so will be biased towards rural areas

# gam_national_trend = gam_national %>%
#   group_by(year) %>%
#   summarise(change_cycles = mean(change_cycles))
#
# saveRDS(gam_national_trend, "gam_national_trend.Rds")
gam_national_trend = readRDS("gam_national_trend.Rds")

gam_national_trend %>%
  ggplot() +
  geom_line(aes(year, change_cycles)) +
  geom_smooth(aes(year, change_cycles)) +
  ylab("Mean cycle count")

# GAM regional
gam_regional_trend = gam_regional %>%
  group_by(year, region) %>%
  summarise(change_cycles = sum(change_cycles))

saveRDS(gam_regional_trend, "gam_regional_trend.Rds")
gam_regional_trend = readRDS("gam_regional_trend.Rds")

gam_regional_trend %>%
  ggplot() +
  geom_line(aes(year, change_cycles, colour = region)) +
  geom_smooth(aes(year, change_cycles)) +
  ylab("Mean cycle count")

# Plot trends together ----------------------------------------------------

# Old version still the best
all_trends = right_join(stats19_national, dft_national_5yr, by = "year") %>%
  left_join(nts_national, by = "year") %>%
  left_join(gam_national_trend, by = "year")

# # New version
# stats19_national2 = stats19_national %>%
#   rename(values = ksi_cycle) %>%
#   mutate(dataset = "stats19")
# nts_national2 = nts_national %>%
#   rename(values = nts_cycles) %>%
#   mutate(dataset = "nts")
# dft_national_5yr2 = dft_national_5yr %>%
#   rename(values = dft_cycles) %>%
#   st_drop_geometry() %>%
#   mutate(dataset = "dft")
# gam_national_trend2 = gam_national_trend %>%
#   rename(values = change_cycles) %>%
#   st_drop_geometry() %>%
#   mutate(dataset = "gam")
#
# all_trends2 = rbind(stats19_national2, dft_national_5yr2, nts_national2, gam_national_trend2)
# summary(all_trends)

# New version graph
# all_trends = all_trends %>%
#   pivot_longer(cols = c(dft_cycles, change_cycles, nts_cycles, ksi_cycle), names_to = "cycle_data")

# Dual axis
ylim.prim = c(0, 120)   # for the DfT and NTS counts
ylim.sec = c(0, 6000)    # for the KSI

b = diff(ylim.prim)/diff(ylim.sec)
a = ylim.prim[1] - b*ylim.sec[1]

ggplot(all_trends, aes(year, dft_cycles)) +
  geom_line(colour = "blue") +
  # geom_smooth(aes(year, dft_cycles), colour = "blue") +
  geom_line(aes(year, nts_cycles), colour = "green") +
  geom_smooth(aes(year, nts_cycles), colour = "green") +
  geom_line(aes(year, change_cycles*100), colour = "yellow") +
  geom_smooth(aes(year, change_cycles*100), colour = "yellow") +
  geom_line(aes(y = a + ksi_cycle*b), color = "red") +
  geom_smooth(aes(y = a + ksi_cycle*b), color = "red") +
  scale_y_continuous("Mean Cycle Count", sec.axis = sec_axis(~ (. - a)/b, name = "Sum Cycle KSI"))
  # + scale_x_continuous("Year", breaks = 1:10) +
  # theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))


# Single axis
all_trends %>%
  ggplot() +
  geom_line(aes(year, ksi_cycle)) +
  geom_smooth(aes(year, ksi_cycle)) +
  geom_line(aes(year, dft_cycles)) +
  geom_smooth(aes(year, dft_cycles), colour = "red") +
  geom_line(aes(year, nts_cycles)) +
  geom_smooth(aes(year, nts_cycles), colour = "green") +
  ylab("Count")

# Graphs with legends ---------------------------------------------

# National risk per bkm
comparisons = all_trends %>%
  mutate(
    dft_norm = dft_cycles / dft_cycles[which(year == 2011)],
    gam_norm = change_cycles / change_cycles[which(year == 2011)],
    nts_norm = nts_cycles / nts_cycles[which(year == 2011)],
    ksi_per_dft = ksi_cycle / dft_cycles,
    ksi_per_gam = ksi_cycle / change_cycles,
    ksi_per_nts = ksi_cycle / nts_cycles * 1000000000,
    dft_risk_norm = ksi_per_dft / ksi_per_dft[which(year == 2011)],
    gam_risk_norm = ksi_per_gam / ksi_per_gam[which(year == 2011)],
    nts_risk_norm = ksi_per_nts / ksi_per_nts[which(year == 2011)]
    )

comparisons2 = comparisons %>%
  pivot_longer(cols = c(dft_risk_norm, gam_risk_norm, nts_risk_norm), names_to = "cycle_volume_data")

comparisons2 %>%
  ggplot(aes(year, value, colour = cycle_volume_data)) +
  geom_line() +
  # geom_smooth(alpha = 0.2) +
  ylab("Cycle KSI risk relative to 2011") +
  labs(x = "Year", colour = "Cycle volume data") +
  # xlim(2010, 2020) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018, 2020), limits = c(2010, 2020)) +
  scale_colour_discrete(labels = c("DfT counters", "GAM", "NTS"))

# Absolute change in cycle flows
comparisons3 = comparisons %>%
  pivot_longer(cols = c(dft_norm, gam_norm, nts_norm), names_to = "cycle_volume")

comparisons3 %>%
  ggplot(aes(year, value, colour = cycle_volume)) +
  geom_line() +
  # geom_smooth(alpha = 0.2) +
  ylab("Cycle volume relative to 2011") +
  labs(x= "Year", colour = "Cycle volume data") +
  scale_colour_discrete(labels = c("DfT counters", "GAM", "NTS"))

# comparisons %>%
#   ggplot() +
#   geom_line(aes(year, dft_risk_norm), colour = "blue") +
#   geom_smooth(aes(year, dft_risk_norm), colour = "blue") +
#   geom_line(aes(year, gam_risk_norm), colour = "red") +
#   geom_smooth(aes(year, gam_risk_norm), colour = "red") +
#   geom_line(aes(year, nts_risk_norm), colour = "green") +
#   geom_smooth(aes(year, nts_risk_norm), colour = "green") +
#   ylab("KSI rate relative to 2011") +
#   xlim(2010, 2020)

# Regional
all_regional = left_join(stats19_regional, dft_regional_5yr, by = c("year", "region")) %>%
  left_join(nts_regional, by = c("year", "region")) %>%
  left_join(gam_regional_trend, by = c("year", "region"))

all_regional = all_regional %>%
  mutate(
    ksi_per_dft = ksi_cycle / dft_cycles,
    ksi_per_gam = ksi_cycle / change_cycles,
    ksi_per_nts = ksi_cycle / nts_cycles * 1000000000
  )

# Regional risk - DfT
all_regional %>%
  ggplot() +
  geom_line(aes(year, ksi_per_dft, colour = region)) +
  geom_smooth(aes(year, ksi_per_dft, colour = region)) +
  ylab("KSI risk per mean cycle count") +
  labs(x = "Year", colour = "Region") +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018)) +
  scale_color_brewer(type = "qual", palette = 3)

# Regional risk - NTS
all_regional %>%
  ggplot() +
  geom_line(aes(year, ksi_per_nts, colour = region), lwd = 0.8) +
  # geom_smooth(aes(year, ksi_per_nts, colour = region), alpha = 0.2) +
  ylab("KSI per Bkm cycled") +
  labs(x = "Year", colour = "Region") +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018)) +
  scale_color_brewer(type = "qual", palette = 3)

# Regional risk - GAM
all_regional %>%
  ggplot() +
  geom_line(aes(year, ksi_per_gam, colour = region)) +
  geom_smooth(aes(year, ksi_per_gam, colour = region)) +
  ylab("KSI risk per mean estimated cycle flow") +
  labs(x = "Year", colour = "Region") +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018)) +
  scale_color_brewer(type = "qual", palette = 3)

# Compare NTS and DfT data at regional level


# Raw DfT counts v collisions

# NTS counts v collisions

# GAM results v collisions
