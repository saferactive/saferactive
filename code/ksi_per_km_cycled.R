library(tidyr)
library(ggplot2)
library(dplyr)
library(tmap)
library(sf)
library(lubridate)

# this just has annual rates per LA. but i want to select peak hour crashes only
# piggyback::pb_download("la_crash_summary_wideform.Rds", tag = "0.1.3")
# crash = readRDS("la_crash_summary_wideform.Rds")

# piggyback::pb_download("la_crash_summary_longform.Rds", tag = "0.1.3")

crash = readRDS("data/crash_2010_2019_with_summary_adjusted_casualties.Rds")

piggyback::pb_download("la_lower.Rds", tag = "0.1.3")
la = readRDS("la_lower.Rds")

crash = st_transform(crash, 27700)
crash = st_join(crash, la)
crash$year = lubridate::year(crash$date)
crash$hour = hour(crash$datetime)

# use peak hours only
crash_wide = crash %>%
  st_drop_geometry() %>%
  filter(hour %in% c(7, 8, 9, 16, 17, 18)) %>%
  group_by(la_name,
           la_code,
           year) %>%
  summarise(ksi_active = sum(casualty_serious_cyclist) +
              sum(casualty_fatal_cyclist) +
              sum(casualty_serious_pedestrian) +
              sum(casualty_fatal_pedestrian)) %>%
  pivot_wider(id_cols = c("la_name","la_code"),
              names_from = c("year"),
              values_from = c("ksi_active"))

piggyback::pb_download("la_lower_km_cycled_2010_2019.csv", tag = "0.1.3")
cycle_km = read.csv("la_lower_km_cycled_2010_2019.csv")
cycle_km = cycle_km[,c("la_code",names(cycle_km)[grepl("km_cycle_20",names(cycle_km))])]

la = left_join(crash_wide, cycle_km, by = c("la_code"))

la$ksi_perMm_2010 = la$`2010` / la$km_cycle_2010 * 1000
la$ksi_perMm_2011 = la$`2011` / la$km_cycle_2011 * 1000
la$ksi_perMm_2012 = la$`2012` / la$km_cycle_2012 * 1000
la$ksi_perMm_2013 = la$`2013` / la$km_cycle_2013 * 1000
la$ksi_perMm_2014 = la$`2014` / la$km_cycle_2014 * 1000
la$ksi_perMm_2015 = la$`2015` / la$km_cycle_2015 * 1000
la$ksi_perMm_2016 = la$`2016` / la$km_cycle_2016 * 1000
la$ksi_perMm_2017 = la$`2017` / la$km_cycle_2017 * 1000
la$ksi_perMm_2018 = la$`2018` / la$km_cycle_2018 * 1000
la$ksi_perMm_2019 = la$`2019` / la$km_cycle_2019 * 1000

la$la_name = sapply(la$la_name, function(x){
  x = strsplit(x, "-")[[1]]
  if(length(x) > 1){
    x = x[2]
  }
  x
}, USE.NAMES = FALSE)

la$la_name = trimws(la$la_name)

# remove extra columns
la_cut = la[,c("la_name",names(la)[grepl("ksi_perMm_",names(la))])]
la_cut = ungroup(la_cut)
#removes Scottish LAs which we don't have estimated cycle flows for
la_cut = la_cut[!is.na(la_cut$ksi_perMm_2019),]
# head(la_cut)

# max and min annual rates in each LA
la_cut$max = apply(la_cut[,names(la_cut)[grepl("ksi_perMm_",names(la_cut))]], 1, max, na.rm = TRUE)
la_cut$min = apply(la_cut[,names(la_cut)[grepl("ksi_perMm_",names(la_cut))]], 1, min, na.rm = TRUE)
la_cut$diff = la_cut$max - la_cut$min
la_cut$diff1019 = la_cut$ksi_perMm_2010 - la_cut$ksi_perMm_2019

# mean rates in each LA
la_cut$mean = apply(la_cut[,names(la_cut)[grepl("ksi_perMm_",names(la_cut))]], 1, mean, na.rm = TRUE)
la_cut$early_mean = apply(la_cut[,names(la_cut)[grepl("ksi_perMm_",names(la_cut))]][,1:5], 1, mean, na.rm = TRUE)
la_cut$late_mean = apply(la_cut[,names(la_cut)[grepl("ksi_perMm_",names(la_cut))]][,6:10], 1, mean, na.rm = TRUE)
la_cut$diffmean = la_cut$late_mean - la_cut$early_mean

# select interesting LAs
top_la = unique(c(
  # top_n(la_cut, 4, diff)$la_name, # greatest difference between min and max rates
  # top_n(la_cut, -4, diff1019)$la_name, # greatest increase in casualty rate from 2010 to 2019
  # top_n(la_cut, 4, diff1019)$la_name, # greatest reduction in casualty rate from 2010 to 2019
  # top_n(la_cut, 4, max)$la_name # greatest max casualty rate

  top_n(la_cut, 4, mean)$la_name, # greatest mean casualty rate
  top_n(la_cut, 4, diffmean)$la_name, # greatest increase in casualty rate from early years to late years
  top_n(la_cut, -4, diffmean)$la_name # greatest decrease in casualty rate from early years to late years
  ))

# top_la = top_n(la_cut, 4, diff)$la_name


la_long = pivot_longer(la_cut,
                        cols = starts_with("ksi_perMm_"),
                        names_prefix = "ksi_perMm_",
                        names_to = "year",
                        values_to = "ksi_perMm"
)

# allow the interesting LAs to be highlighted in the plot
la_long$sel = la_long$la_name %in% top_la
la_long$year = as.integer(la_long$year)
la_long$la_name_plot = ifelse(la_long$sel, la_long$la_name, NA)
head(la_long)

ggplot(la_long, aes(year, ksi_perMm, colour = la_name_plot, group = la_name)) +
  geom_line(data = subset(la_long, sel == FALSE), aes(size = sel)) +
  geom_line(data = subset(la_long, sel == TRUE), aes(size = sel)) +
  ylab("Active Traveler KSI per 1000 km cycled") +
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



# ggplot(crash_yr,
#        aes(year, active_ksi_per100k_work, colour = la_name_plot, group = la_name)) +
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
piggyback::pb_download("la_lower_for_plots.Rds")
bounds = readRDS("la_lower_for_plots.Rds")
bounds = left_join(bounds, la, by = "la_code")
bounds = bounds[!is.na(bounds$ksi_perMm_2019),]
# bounds10 = bounds[is.na(bounds$ksi_perMm_2010),]

t1 = tm_shape(bounds) +
  tm_fill("ksi_perMm_2010") +
  tm_borders(lwd = 0.1) +
  tm_layout(legend.outside = TRUE)

tm_shape(bounds) +
  tm_fill("ksi_perMm_2011") +
  tm_borders(lwd = 0.1) +
  tm_layout(legend.outside = TRUE)


piggyback::pb_download("la_upper_for_plots.Rds") #not found
bounds_upper = readRDS("la_upper_for_plots.Rds")
