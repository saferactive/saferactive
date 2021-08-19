library(tidyverse)
library(sf)
library(mapview)

# Input data --------------------------------------------------------------

# Simplified English regions
regions = readRDS("regions_renamed.Rds") %>%
  st_transform(27700)

# DfT AADF traffic count data 2000-2020
dft_counts = readRDS("traffic_joined.Rds")

# NTS travel data 2003-2019
nts_clean = read.csv("d_region_clean.csv")

# Stats19 collision data 2010-2019
collision_data = readRDS("data/crash_2010_2019_with_summary_adjusted_casualties.Rds") %>%
  mutate(ksi_cycle = casualty_serious_cyclist + casualty_fatal_cyclist) %>%
  st_transform(27700)
collision_data$year = lubridate::year(collision_data$date)

# GAM model raw results for changes in traffic counts 2010-2019
gam_results = readRDS("gam-full-results-grid-national.Rds")

# # GAM results aggregated by lower tier LA
# gam_by_la = read.csv("la_lower_km_cycled_2010_2019.csv")


# Single dataset trends ---------------------------------------------------


# DfT AADF counts ---------------------------------------------------------

dft_national = dft_counts %>%
  group_by(year) %>%
  summarise(dft_cycles = mean(pedal_cycles))

dft_national %>%
  ggplot() +
  geom_line(aes(year, dft_cycles)) +
  geom_smooth(aes(year, dft_cycles)) +
  ylab("Mean cycle AADF")

dft_regional = st_join(dft_counts, regions)

dft_regional = dft_regional %>%
  st_drop_geometry() %>%
  group_by(RGN20CD, region, year) %>%
  summarise(dft_cycles = mean(pedal_cycles))
summary(dft_regional)

dft_regional %>%
  ggplot() +
  geom_line(aes(year, dft_cycles, colour = region)) +
  geom_smooth(aes(year, dft_cycles)) +
  ylab("Mean cycle AADF")

# More reliable DfT data

# points measured every single year 2010-2019
dft_10yr = dft_counts %>%
  filter(year %in% 2010:2019) %>%
  group_by(count_point_id) %>%
  mutate(n = n()) %>%
  filter(n > 9)
length(unique(dft_10yr$count_point_id)) # 3045

dft_national_10yr = dft_counts %>%
  filter(count_point_id %in% dft_10yr$count_point_id) %>%
  group_by(year) %>%
  summarise(dft_cycles = mean(pedal_cycles))

# points measured at least 5 times 2010-2019
dft_5yr = dft_counts %>%
  filter(year %in% 2010:2019) %>%
  group_by(count_point_id) %>%
  mutate(n = n()) %>%
  filter(n > 4)
length(unique(dft_5yr$count_point_id)) # 3045

dft_national_5yr = dft_counts %>%
  filter(count_point_id %in% dft_5yr$count_point_id) %>%
  group_by(year) %>%
  summarise(dft_cycles = mean(pedal_cycles))



# NTS results -------------------------------------------------------------

# convert this to % increase from 2011
#normalise then run lm
nts_national = nts_clean %>%
  group_by(year) %>%
  summarise(nts_cycles = mean(bicycle))

nts_national %>%
  ggplot() +
  geom_line(aes(year, nts_cycles)) +
  geom_smooth(aes(year, nts_cycles)) +
  ylab("Distance cycled/yr (km)")

# regional
nts_regional = nts_clean %>%
  group_by(region, year) %>%
  summarise(nts_cycles = mean(bicycle))
summary(nts_regional)

nts_regional %>%
  ggplot() +
  geom_line(aes(year, nts_cycles, colour = region)) +
  geom_smooth(aes(year, nts_cycles)) +
  ylab("Mean cycle journeys")


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
# stats19_regional = st_join(collision_data, regions)
#
# stats19_regional = stats19_regional %>%
#   st_drop_geometry() %>%
#   group_by(RGN20CD, region, year) %>%
#   summarise(ksi_cycle = sum(ksi_cycle))
# summary(stats19_regional)
#
# saveRDS(stats19_regional, "stats19_regional.Rds")
stats19_regional = readRDS("stats19_regional.Rds")

stats19_regional %>%
  ggplot() +
  geom_line(aes(year, ksi_cycle, colour = region)) +
  geom_smooth(aes(year, ksi_cycle)) +
  ylab("Sum cycle ksi")


# GAM results -------------------------------------------------------------

gam_national = gam_results %>%
  st_as_sf(coords = c("easting", "northing"), crs = 27700)
gam_regional = st_join(gam_national, regions) #doesnt work produces NAs

# this doesn't weight for population so will be biased towards rural areas
gam_national_trend = gam_national %>%
  group_by(year) %>%
  summarise(change_cycles = mean(change_cycles))

gam_national_trend %>%
  ggplot() +
  geom_line(aes(year, change_cycles)) +
  geom_smooth(aes(year, change_cycles)) +
  ylab("Mean cycle count")

saveRDS(gam_national_trend, "gam_national_trend.Rds")
gam_national_trend = readRDS("gam_national_trend.Rds")

# Plot trends together ----------------------------------------------------

all_trends = left_join(stats19_national, dft_national_5yr, by = "year") %>%
  left_join(nts_national, by = "year") %>%
  left_join(gam_national_trend, by = "year")
summary(all_trends)

# Dual axis
ylim.prim = c(0, 120)   # for the DfT and NTS counts
ylim.sec = c(0, 6000)    # for the KSI

b = diff(ylim.prim)/diff(ylim.sec)
a = ylim.prim[1] - b*ylim.sec[1]

ggplot(all_trends, aes(year, dft_cycles)) +
  geom_line(colour = "blue") +
  geom_smooth(aes(year, dft_cycles), colour = "blue") +
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

# Risk per bkm --------------------------------------------------------

# National
comparisons = all_trends %>%
  mutate(
    ksi_per_dft = ksi_cycle / dft_cycles,
    ksi_per_gam = ksi_cycle / change_cycles,
    ksi_per_nts = ksi_cycle / nts_cycles
    )

comparisons %>%
  ggplot() +
  geom_line(aes(year, ksi_per_dft)) +
  geom_smooth(aes(year, ksi_per_dft)) +
  geom_line(aes(year, ksi_per_gam/100)) +
  geom_smooth(aes(year, ksi_per_gam/100), colour = "red") +
  geom_line(aes(year, ksi_per_nts)) +
  geom_smooth(aes(year, ksi_per_nts), colour = "green") +
  ylab("KSI rate relative to 2011")

# Regional
all_regional = left_join(stats19_regional, dft_regional, by = c("year", "region")) %>%
  left_join(nts_regional, by = c("year", "region")) %>%
  left_join(gam_regional_trend, by = c("year", "region"))

all_regional = all_regional %>%
  mutate(
    ksi_per_dft = ksi_cycle / dft_cycles,
    # ksi_per_gam = ksi_cycle / change_cycles,
    ksi_per_nts = ksi_cycle / nts_cycles
  )

# Regional risk - DfT

# Regional risk - NTS
all_regional %>%
  ggplot() +
  geom_line(aes(year, ksi_per_nts, colour = region)) +
  geom_smooth(aes(year, ksi_per_nts)) +
  ylab("Mean cycle journeys")

# Regional risk - GAM


# Compare NTS and DfT data at regional level


# Raw DfT counts v collisions

# NTS counts v collisions

# GAM results v collisions
