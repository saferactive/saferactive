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
  mutate(ksi_cycle = casualty_serious_cyclist + casualty_fatal_cyclist)
collision_data$year = lubridate::year(collision_data$date)

# GAM model raw results for changes in traffic counts
gam_results = readRDS("gam-full-results-grid-national.Rds")

# GAM results aggregated by lower tier LA
gam_by_la = read.csv("la_lower_km_cycled_2010_2019.csv")


# Single dataset trends ---------------------------------------------------

# DfT AADF counts
dft_national = dft_counts %>%
  group_by(year) %>%
  summarise(dft_cycles = mean(pedal_cycles))

dft_national %>%
  ggplot() +
  geom_line(aes(year, dft_cycles)) +
  geom_smooth(aes(year, dft_cycles)) +
  ylab("Mean cycle AADF")

# NTS counts
nts_national = nts_clean %>%
  group_by(year) %>%
  summarise(nts_cycles = mean(bicycle))

nts_national %>%
  ggplot() +
  geom_line(aes(year, nts_cycles)) +
  geom_smooth(aes(year, nts_cycles)) +
  ylab("Distance cycled/yr (km)")

# Collisions
stats19_national = collision_data %>%
  st_drop_geometry() %>%
  group_by(year) %>%
  summarise(ksi_cycle = sum(ksi_cycle))

stats19_national %>%
  ggplot() +
  geom_line(aes(year, ksi_cycle)) +
  geom_smooth(aes(year, ksi_cycle)) +
  ylab("Cycle ksi casualties/yr")


# Plot trends together ----------------------------------------------------

all_trends = inner_join(stats19_national, dft_national, by = "year") %>%
  inner_join(nts_national, by = "year")
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
  geom_line(aes(y = a + ksi_cycle*b), color = "red") +
  geom_smooth(aes(y = a + ksi_cycle*b), color = "red") +
  scale_y_continuous("Mean Cycle Count", sec.axis = sec_axis(~ (. - a)/b, name = "Sum Cycle KSI")) +
  # scale_x_continuous("Year", breaks = 1:10) +
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

# Data comparisons --------------------------------------------------------

dft_regional = st_join(dft_counts, regions)

summaries = dft_regional %>%
  group_by(RGN20CD, region) %>%
  summarise(pedal_cyles = mean(pedal_cycles))
summaries

# Compare NTS and DfT data at regional level


# Raw DfT counts v collisions

# NTS counts v collisions

# GAM results v collisions
