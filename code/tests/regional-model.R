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

# Get more reliable DfT data

# points measured at least 5 times 2010-2019
dft_5yr = dft_counts %>%
  filter(year %in% 2010:2019) %>%
  group_by(count_point_id) %>%
  mutate(n = n()) %>%
  filter(n > 4)
length(unique(dft_5yr$count_point_id)) # 5719

# points measured every single year 2010-2019
dft_10yr = dft_counts %>%
  filter(year %in% 2010:2019) %>%
  group_by(count_point_id) %>%
  mutate(n = n()) %>%
  filter(n > 9)
length(unique(dft_10yr$count_point_id)) # 3045

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

dft_regional = st_join(dft_counts, regions)

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
  ylab("Mean cycle AADF")

dft_regional_all %>%
  filter(region != "London") %>%
  ggplot() +
  geom_line(aes(year, dft_cycles, colour = region)) +
  # geom_smooth(aes(year, dft_cycles)) +
  ylab("Mean cycle AADF")

dft_regional_10yr %>%
  filter(region != "London") %>%
  ggplot() +
  geom_line(aes(year, dft_cycles, colour = region)) +
  # geom_smooth(aes(year, dft_cycles)) +
  ylab("Mean cycle AADF")

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

gam_regional = st_join(gam_national, regions) #produces NAs - where are these points?

saveRDS(gam_regional, "gam_regional.Rds")

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
  summarise(change_cycles = mean(change_cycles))

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
summary

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
    ksi_per_nts = ksi_cycle / nts_cycles,
    dft_risk_norm = ksi_per_dft / ksi_per_dft[which(year == 2011)],
    gam_risk_norm = ksi_per_gam / ksi_per_gam[which(year == 2011)],
    nts_risk_norm = ksi_per_nts / ksi_per_nts[which(year == 2011)]
    )

comparisons2 = comparisons %>%
  pivot_longer(cols = c(dft_risk_norm, gam_risk_norm, nts_risk_norm), names_to = "cycle_volume_data")

comparisons2 %>%
  ggplot(aes(year, value, colour = cycle_volume_data)) +
  geom_line() +
  geom_smooth(alpha = 0.2) +
  ylab("Cycle KSI rate relative to 2011") +
  xlim(2010, 2020)

# Absolute change in cycle flows
comparisons3 = comparisons %>%
  pivot_longer(cols = c(dft_norm, gam_norm, nts_norm), names_to = "cycle_volume")

comparisons3 %>%
  ggplot(aes(year, value, colour = cycle_volume)) +
  geom_line() +
  # geom_smooth(alpha = 0.2) +
  ylab("Cycle volume relative to 2011")

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
    ksi_per_nts = ksi_cycle / nts_cycles
  )

# Regional risk - DfT
all_regional %>%
  ggplot() +
  geom_line(aes(year, ksi_per_dft, colour = region)) +
  geom_smooth(aes(year, ksi_per_dft, colour = region)) +
  ylab("KSI risk per mean cycle count")

# Regional risk - NTS
all_regional %>%
  ggplot() +
  geom_line(aes(year, ksi_per_nts, colour = region)) +
  geom_smooth(aes(year, ksi_per_nts, colour = region)) +
  ylab("KSI risk per journey")

# Regional risk - GAM
all_regional %>%
  ggplot() +
  geom_line(aes(year, ksi_per_gam, colour = region)) +
  geom_smooth(aes(year, ksi_per_gam, colour = region)) +
  ylab("KSI risk per mean estimated cycle flow")

# Compare NTS and DfT data at regional level


# Raw DfT counts v collisions

# NTS counts v collisions

# GAM results v collisions
