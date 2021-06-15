library(tidyr)
library(ggplot2)
library(dplyr)
library(tmap)
library(sf)
library(lubridate)
library(readr)

## this just has annual rates per LA. but i want to select peak hour crashes only
# piggyback::pb_download("la_crash_summary_wideform.Rds", tag = "0.1.3")
# crash = readRDS("la_crash_summary_wideform.Rds")
# piggyback::pb_download("la_crash_summary_longform.Rds", tag = "0.1.3")

# piggyback::pb_download("crash_2010_2019_with_summary_adjusted_casualties", tag = "0.1.4")
# crash_raw = readRDS("crash_2010_2019_with_summary_adjusted_casualties.Rds")

crash_raw = readRDS("data/crash_2010_2019_with_summary_adjusted_casualties.Rds")

crash_raw = st_drop_geometry(crash_raw)

# # Correct location error
# # View(crash_raw %>% filter(local_authority_district == "Dover" & police_force == "Thames Valley"))
# crash_raw$local_authority_district[crash_raw$accident_index == "2018430248885"] = "Windsor and Maidenhead"
# crash_raw$local_authority_highway[crash_raw$accident_index == "2018430248885"] = "Windsor and Maidenhead"
#
# saveRDS(crash_raw, "data/crash_2010_2019_with_summary_adjusted_casualties.Rds")

# Get and apply LAD codes (joining using old LAD names)
names_lad = read_csv("Local_Authority_Districts_(December_2019)_Names_and_Codes_in_the_United_Kingdom_updated.csv")

namejoin = left_join(crash_raw, names_lad, by = c("local_authority_district" = "LAD19NM"))


# Check no rows are mising codes
xx = namejoin %>% filter(is.na(LAD19CD)) %>% group_by(local_authority_district) %>% summarise()

# Standardise to new LAD names
new_names = read_csv("Local_Authority_Districts_(December_2019)_Names_and_Codes_in_the_United_Kingdom.csv") %>%
  select(-LAD19NMW, -FID)

nameagain = inner_join(new_names, namejoin, by = "LAD19CD")

# Check no rows are mising codes
xx = nameagain %>% filter(is.na(LAD19NM)) %>% group_by(local_authority_district) %>% summarise()
length(unique(nameagain$local_authority_district)) #380
length(unique(nameagain$LAD19NM)) #367
length(unique(nameagain$LAD19CD)) #367

## LA boundaries
# bfc = read_sf("Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BFC.shp")

# Get LAD geometry
# piggyback::pb_download("la_lower.Rds", tag = "0.1.3")
la = readRDS("la_lower.Rds")

# # old method (which introduced geographical errors)
# crash = st_transform(crash, 27700)
# crash = st_join(crash, la)

# new method
crash = right_join(la, nameagain, by = c("la_code" = "LAD19CD"))

xx = crash %>% st_drop_geometry() %>% filter(is.na(la_name))
unique(xx$LAD19NM)

crash$year = lubridate::year(crash$date)
crash$hour = lubridate::hour(crash$datetime)
crash$la_name = NULL
crash$local_authority_district = NULL

# piggyback::pb_download("la_lower_km_cycled_2010_2019.csv", tag = "0.1.3")
cycle_km = read.csv("la_lower_km_cycled_2010_2019.csv") %>% select(-la_name)
cycle_km = cycle_km[,c("la_code",names(cycle_km)[grepl("km_cycle_20",names(cycle_km))])]

# Police forces and LA names now match up
pf_lookup = crash %>%
  st_drop_geometry() %>%
  select(la_code, LAD19NM, police_force) %>%
  group_by(la_code, LAD19NM, police_force) %>%
  filter(n() > 3) %>%
  summarise()
# pf_lookup[duplicated(pf_lookup$LAD19NM),]

# Group by lower tier LA --------------------------------------------------

# use peak hours on weekdays only
crash_wide = crash %>%
  st_drop_geometry() %>%
  filter(
    hour %in% c(7, 8, 9, 16, 17, 18),
    ! day_of_week %in% c("Saturday", "Sunday")
    ) %>%
  group_by(LAD19NM,
           la_code,
           year) %>%
  summarise(ksi_cycle = sum(casualty_serious_cyclist) +
              sum(casualty_fatal_cyclist)) %>%
  pivot_wider(id_cols = c("LAD19NM","la_code"),
              names_from = c("year"),
              values_from = c("ksi_cycle"),
              names_prefix = "ksi_")

la = left_join(crash_wide, cycle_km, by = "la_code")

la$ksi_perMm_2010 = la$ksi_2010 / la$km_cycle_2010 * 1000
la$ksi_perMm_2011 = la$ksi_2011 / la$km_cycle_2011 * 1000
la$ksi_perMm_2012 = la$ksi_2012 / la$km_cycle_2012 * 1000
la$ksi_perMm_2013 = la$ksi_2013 / la$km_cycle_2013 * 1000
la$ksi_perMm_2014 = la$ksi_2014 / la$km_cycle_2014 * 1000
la$ksi_perMm_2015 = la$ksi_2015 / la$km_cycle_2015 * 1000
la$ksi_perMm_2016 = la$ksi_2016 / la$km_cycle_2016 * 1000
la$ksi_perMm_2017 = la$ksi_2017 / la$km_cycle_2017 * 1000
la$ksi_perMm_2018 = la$ksi_2018 / la$km_cycle_2018 * 1000
la$ksi_perMm_2019 = la$ksi_2019 / la$km_cycle_2019 * 1000

# No longer needed
# la$la_name = sapply(la$la_name, function(x){
#   x = strsplit(x, "-")[[1]]
#   if(length(x) > 1){
#     x = x[2]
#   }
#   x
# }, USE.NAMES = FALSE)
#
# la_name2 = trimws(la$la_name)

saveRDS(la, "cycle-collision-risk.Rds")


# Group by police force ---------------------------------------------------

# use peak hours on weekdays only
crash_pf = crash %>%
  st_drop_geometry() %>%
  filter(
    hour %in% c(7, 8, 9, 16, 17, 18),
    ! day_of_week %in% c("Saturday", "Sunday")
  ) %>%
  group_by(police_force,
           year) %>%
  summarise(ksi_cycle = sum(casualty_serious_cyclist) +
              sum(casualty_fatal_cyclist)) %>%
  pivot_wider(id_cols = c("police_force"),
              names_from = c("year"),
              values_from = c("ksi_cycle"),
              names_prefix = "ksi_")

cycle_km_pf = inner_join(cycle_km, pf_lookup, by = "la_code")
cycle_km_pf = cycle_km_pf %>%
  group_by(police_force) %>%
  summarise(
    km_cycle_2010 = sum(km_cycle_2010),
    km_cycle_2011 = sum(km_cycle_2011),
    km_cycle_2012 = sum(km_cycle_2012),
    km_cycle_2013 = sum(km_cycle_2013),
    km_cycle_2014 = sum(km_cycle_2014),
    km_cycle_2015 = sum(km_cycle_2015),
    km_cycle_2016 = sum(km_cycle_2016),
    km_cycle_2017 = sum(km_cycle_2017),
    km_cycle_2018 = sum(km_cycle_2018),
    km_cycle_2019 = sum(km_cycle_2019),
            )

la_pf = left_join(crash_pf, cycle_km_pf, by = c("police_force"))

la_pf$ksi_perMm_2010 = la_pf$ksi_2010 / la_pf$km_cycle_2010 * 1000
la_pf$ksi_perMm_2011 = la_pf$ksi_2011 / la_pf$km_cycle_2011 * 1000
la_pf$ksi_perMm_2012 = la_pf$ksi_2012 / la_pf$km_cycle_2012 * 1000
la_pf$ksi_perMm_2013 = la_pf$ksi_2013 / la_pf$km_cycle_2013 * 1000
la_pf$ksi_perMm_2014 = la_pf$ksi_2014 / la_pf$km_cycle_2014 * 1000
la_pf$ksi_perMm_2015 = la_pf$ksi_2015 / la_pf$km_cycle_2015 * 1000
la_pf$ksi_perMm_2016 = la_pf$ksi_2016 / la_pf$km_cycle_2016 * 1000
la_pf$ksi_perMm_2017 = la_pf$ksi_2017 / la_pf$km_cycle_2017 * 1000
la_pf$ksi_perMm_2018 = la_pf$ksi_2018 / la_pf$km_cycle_2018 * 1000
la_pf$ksi_perMm_2019 = la_pf$ksi_2019 / la_pf$km_cycle_2019 * 1000

# No longer needed
# la$la_name = sapply(la$la_name, function(x){
#   x = strsplit(x, "-")[[1]]
#   if(length(x) > 1){
#     x = x[2]
#   }
#   x
# }, USE.NAMES = FALSE)
#
# la$la_name = trimws(la$la_name)

saveRDS(la_pf, "cycle-collision-risk-pf.Rds")


# Read in results ---------------------------------------------------------

la = readRDS("cycle-collision-risk.Rds")
la_pf = readRDS("cycle-collision-risk-pf.Rds")

# remove extra columns
# la_cut = la[,c("LAD19NM",names(la)[grepl("ksi_perMm_",names(la))])]
la = ungroup(la)
la_pf = ungroup(la_pf)

#removes Scottish LAs which we don't have estimated cycle flows for
la = la[!is.na(la$ksi_perMm_2019),]
la_pf = la_pf[!is.na(la_pf$ksi_perMm_2019),]
# head(la)


# Get LAD populations -----------------------------------------------------
# piggyback::pb_list()
piggyback::pb_download("MYEB1_detailed_population_estimates_series_UK_.2019_geog20.csv", tag = "0.1.4")
pop1 = read_csv("MYEB1_detailed_population_estimates_series_UK_.2019_geog20.csv")

names(pop1)
# View(pop1)

pop2 = pop1 %>%
  group_by(ladcode20, laname20) %>%
  summarise(
    population_2010 = sum(population_2010),
    population_2011 = sum(population_2011),
    population_2012 = sum(population_2012),
    population_2013 = sum(population_2013),
    population_2014 = sum(population_2014),
    population_2015 = sum(population_2015),
    population_2016 = sum(population_2016),
    population_2017 = sum(population_2017),
    population_2018 = sum(population_2018),
    population_2019 = sum(population_2019),
  )

la = left_join(la, pop2, by = c("la_code" = "ladcode20"))

# check for NAs
xx = la_pop %>%
  filter(is.na(laname20))

# Calculate km_cycled per capita in each year
la = la %>%
  mutate(
    km_percap_2010 = km_cycle_2010 / population_2010,
    km_percap_2011 = km_cycle_2011 / population_2011,
    km_percap_2012 = km_cycle_2012 / population_2012,
    km_percap_2013 = km_cycle_2013 / population_2013,
    km_percap_2014 = km_cycle_2014 / population_2014,
    km_percap_2015 = km_cycle_2015 / population_2015,
    km_percap_2016 = km_cycle_2016 / population_2016,
    km_percap_2017 = km_cycle_2017 / population_2017,
    km_percap_2018 = km_cycle_2018 / population_2018,
    km_percap_2019 = km_cycle_2019 / population_2019
  )

# # max and min annual rates in each LA
# la$max = apply(la[,names(la)[grepl("ksi_perMm_",names(la))]], 1, max, na.rm = TRUE)
# la$min = apply(la[,names(la)[grepl("ksi_perMm_",names(la))]], 1, min, na.rm = TRUE)
# la$diff = la$max - la$min
# la$diff1019 = la$ksi_perMm_2010 - la$ksi_perMm_2019

# mean rates in each LA
la$mean = apply(la[,names(la)[grepl("ksi_perMm_",names(la))]], 1, mean, na.rm = TRUE)
la$early_mean = apply(la[,names(la)[grepl("ksi_perMm_",names(la))]][,1:5], 1, mean, na.rm = TRUE)
la$late_mean = apply(la[,names(la)[grepl("ksi_perMm_",names(la))]][,6:10], 1, mean, na.rm = TRUE)
la$diffmean = la$late_mean - la$early_mean
la$mean_km_cycled = apply(la[,names(la)[grepl("km_cycle_",names(la))]], 1, mean, na.rm = TRUE)
la$mean_cycle_ksi = apply(la[,names(la)[grepl("ksi_20",names(la))]], 1, mean, na.rm = TRUE)
la$mean_km_percap = apply(la[,names(la)[grepl("km_percap_",names(la))]], 1, mean, na.rm = TRUE)

# mean rates in each LA
la_pf$mean = apply(la_pf[,names(la_pf)[grepl("ksi_perMm_",names(la_pf))]], 1, mean, na.rm = TRUE)
la_pf$early_mean = apply(la_pf[,names(la_pf)[grepl("ksi_perMm_",names(la_pf))]][,1:5], 1, mean, na.rm = TRUE)
la_pf$late_mean = apply(la_pf[,names(la_pf)[grepl("ksi_perMm_",names(la_pf))]][,6:10], 1, mean, na.rm = TRUE)
la_pf$diffmean = la_pf$late_mean - la_pf$early_mean

# select interesting LAs
top_la = unique(c(
  # top_n(la, 4, diff)$LAD19NM, # greatest difference between min and max rates
  # top_n(la, -4, diff1019)$LAD19NM, # greatest increase in casualty rate from 2010 to 2019
  # top_n(la, 4, diff1019)$LAD19NM, # greatest reduction in casualty rate from 2010 to 2019
  # top_n(la, 4, max)$LAD19NM # greatest max casualty rate

  top_n(la, 4, mean)$LAD19NM, # greatest mean casualty rate
  top_n(la, -4, mean)$LAD19NM, # lowest mean casualty rate
  top_n(la, 4, diffmean)$LAD19NM, # greatest increase in casualty rate from early years to late years
  top_n(la, -4, diffmean)$LAD19NM # greatest decrease in casualty rate from early years to late years
  ))

top_la_pf = unique(c(
  top_n(la_pf, 4, mean)$police_force, # greatest mean casualty rate
  top_n(la_pf, -4, mean)$police_force, # lowest mean casualty rate
  top_n(la_pf, 4, diffmean)$police_force, # greatest increase in casualty rate from early years to late years
  top_n(la_pf, -4, diffmean)$police_force # greatest decrease in casualty rate from early years to late years
))

# top_la = top_n(la, 4, diff)$LAD19NM


la_long = pivot_longer(la,
                        cols = starts_with("ksi_perMm_"),
                        names_prefix = "ksi_perMm_",
                        names_to = "year",
                        values_to = "ksi_perMm"
)

la_pf_long = pivot_longer(la_pf,
                       cols = starts_with("ksi_perMm_"),
                       names_prefix = "ksi_perMm_",
                       names_to = "year",
                       values_to = "ksi_perMm"
)

# allow the interesting LAs to be highlighted in the plot
la_long$sel = la_long$LAD19NM %in% top_la
la_long$year = as.integer(la_long$year)
la_long$LAD19NM_plot = ifelse(la_long$sel, la_long$LAD19NM, NA)
head(la_long)

la_pf_long$sel = la_pf_long$police_force %in% top_la_pf
la_pf_long$year = as.integer(la_pf_long$year)
la_pf_long$police_force_plot = ifelse(la_pf_long$sel, la_pf_long$police_force, NA)
head(la_pf_long)

# save la_long for future reference
saveRDS(la_long, "la_long.Rds")

ggplot(la_long, aes(year, ksi_perMm, colour = LAD19NM_plot, group = LAD19NM)) +
  geom_line(data = subset(la_long, sel == FALSE), aes(size = sel)) +
  geom_line(data = subset(la_long, sel == TRUE), aes(size = sel)) +
  ylab("KSI per 1000 km cycled") +
  labs(color = "Local Authority") +
  guides(size = FALSE) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = 0.1)) +
  # scale_color_manual(values=c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99",
  #                             "#E31A1C",
  #                             "#d3d3d3","#FDBF6F",
  #                             "#FF7F00", "#CAB2D6", "#6A3D9A", "#CAB2D6")) +
  scale_x_continuous(breaks = 2010:2019, expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle("KSI Risk, Selected LAs")

ggplot(la_pf_long, aes(year, ksi_perMm, colour = police_force_plot, group = police_force)) +
  geom_line(data = subset(la_pf_long, sel == FALSE), aes(size = sel)) +
  geom_line(data = subset(la_pf_long, sel == TRUE), aes(size = sel)) +
  ylab("KSI per 1000 km cycled") +
  labs(color = "Police Force") +
  guides(size = FALSE) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = 0.1)) +
  # scale_color_manual(values=c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99",
  #                             "#E31A1C",
  #                             "#d3d3d3","#FDBF6F",
  #                             "#FF7F00", "#CAB2D6", "#6A3D9A", "#CAB2D6")) +
  scale_x_continuous(breaks = 2010:2019, expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 2)) +
  ggtitle("KSI Risk, Selected Police Forces")

# this should be km cycled per capita, or as a % of travel to work, not absolute km cycled
# but city of london is way too high. use workplace population?
ggplot(la, aes(x = mean_km_cycled, y = mean_cycle_ksi)) +
  geom_point() +
  geom_text(label = la$LAD19NM, nudge_y = -1.5, cex = 2.8) +
  labs(x = "Mean km (1000s) cycled", y = "Mean cycle KSI")

ggplot(la, aes(x = mean_km_cycled, y = mean)) +
  geom_point()

ggplot(la, aes(x = mean_km_percap, y = mean)) +
  geom_point() +
  xlim(0, 0.5) +
  ylim(0, 3) +
  labs(x = "Mean km (1000s) cycled per capita (resident pop'n)", y = "Mean cycle KSI per Mkm cycled")
    # + geom_text(label = la$LAD19NM, nudge_y = +0.1, cex = 2)

# ggplot(crash_yr,
#        aes(year, active_ksi_per100k_work, colour = LAD19NM_plot, group = LAD19NM)) +
#   geom_line(data = subset(crash_yr, lwd == FALSE), aes(size = lwd)) +
#   geom_line(data = subset(crash_yr, lwd == TRUE), aes(size = lwd)) +
#   ylab("Active Travel KSI per 100k workplace population") +
#   labs(color = "Local Authority") +
#   guides(size = FALSE) +
#   guides(colour = guide_legend(override.aes = list(size=3))) +
#   scale_size_manual(values = c("TRUE" = 1.5, "FALSE" = 0.1)) +
#   scale_color_manual(values=c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99",
#                               "#E31A1C",
#                               "#d3d3d3","#FDBF6F",
#                               "#FF7F00", "#CAB2D6", "#6A3D9A")) +
#   scale_x_continuous(breaks = 2010:2019, expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   ggtitle("Top and Bottom LAs")

# get the geometry again
# piggyback::pb_download("la_lower_for_plots.Rds", tag = "0.1.3")
bounds = readRDS("la_lower_for_plots.Rds")
bounds = left_join(bounds %>% select(-la_name), la, by = "la_code")
bounds = bounds[!is.na(bounds$ksi_perMm_2019),]
# bounds10 = bounds[is.na(bounds$ksi_perMm_2010),]

# Police force geometry
pf_geom = read_sf("Police_Force_Areas_(December_2018)_EW_BUC.geojson") # todo: make reproducible
pf_geom$pfa18nm[pf_geom$pfa18nm == "Devon & Cornwall"] = "Devon and Cornwall"
pf_geom$pfa18nm[pf_geom$pfa18nm == "London, City of"] = "City of London"
pf_geom = left_join(pf_geom, la_pf, by = c("pfa18nm" = "police_force"))
# pf_geom = pf_geom[!is.na(pf_geom$ksi_perMm_2019),]

tm_shape(bounds) +
  tm_fill("ksi_perMm_2019") +
  tm_borders(lwd = 0.1) +
  tm_layout(legend.outside = TRUE)

tm_shape(pf_geom) +
  tm_fill("ksi_perMm_2019", breaks = c(0, 0.4, 0.8, 1.2, 1.6, 2)) +
  tm_borders(lwd = 0.1) +
  tm_layout(legend.outside = TRUE)

####

t1 = tm_shape(bounds) +
  tm_fill("mean", breaks = c(0, 0.5, 1, 1.5, 2, 3, 4), title = "2010-19") +
  tm_borders(lwd = 0.1)
  # + tm_layout(title = "Cycle KSI/Mkm 2010-19")

t2 = tm_shape(bounds) +
  tm_fill("early_mean", breaks = c(0, 0.5, 1, 1.5, 2, 3, 4), title = "2010-14") +
  tm_borders(lwd = 0.1)
 # + tm_layout(title = "Cycle KSI/Mkm 2010-14")

t3 = tm_shape(bounds) +
  tm_fill("late_mean", breaks = c(0, 0.5, 1, 1.5, 2, 3, 4), title = "2015-19") +
  tm_borders(lwd = 0.1)
  # + tm_layout(title = "Cycle KSI/Mkm 2015-19")

tmap_arrange(t1, t2, t3)

####

t1 = tm_shape(pf_geom) +
  tm_fill("mean", breaks = c(0, 0.4, 0.8, 1.2, 1.6, 2), title = "2010-19") +
  tm_borders(lwd = 0.1)
# + tm_layout(title = "Cycle KSI/Mkm 2010-19")

t2 = tm_shape(pf_geom) +
  tm_fill("early_mean", breaks = c(0, 0.4, 0.8, 1.2, 1.6, 2), title = "2010-14") +
  tm_borders(lwd = 0.1)
# + tm_layout(title = "Cycle KSI/Mkm 2010-14")

t3 = tm_shape(pf_geom) +
  tm_fill("late_mean", breaks = c(0, 0.4, 0.8, 1.2, 1.6, 2), title = "2015-19") +
  tm_borders(lwd = 0.1)
# + tm_layout(title = "Cycle KSI/Mkm 2015-19")

tmap_arrange(t1, t2, t3)




# piggyback::pb_download("la_upper_for_plots.Rds") #not found
# bounds_upper = readRDS("la_upper_for_plots.Rds")

## Histogram of LA values

hist(bounds$mean, xlim = c(0, 4), breaks = 15)
hist(bounds$early_mean, xlim = c(0, 4), breaks = 20)
hist(bounds$late_mean, xlim = c(0, 4), breaks = 15)

# top 10 lists
highest_mean = arrange(top_n(la, 10, mean),-mean) # greatest mean casualty rate

lowest_mean = arrange(top_n(la, 10, -mean),mean) # lowest mean casualty rate

greatest_increase = arrange(top_n(la, 10, diffmean),-diffmean) # greatest increase in casualty rate

greatest_decrease = arrange(top_n(la, 10, -diffmean),diffmean) # greatest decrease in casualty rate

