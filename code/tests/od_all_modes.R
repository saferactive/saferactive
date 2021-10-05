# 2011 pct od data for all modes

library(pct)
national_od = get_od(type = "from", omit_intrazonal = FALSE)

# All commutes from origin LA
national_origin_la = national_od %>%
  group_by(la_1) %>%
  summarise(bicycle = sum(bicycle),
            foot = sum(foot),
            car_driver = sum(car_driver),
            all = sum(all))
national_origin_la = national_origin_la %>%
  rename(bicycle_o = bicycle,
         foot_o = foot,
         car_driver_o = car_driver,
         all_o = all)

# All commutes to destination LA
national_dest_la = national_od %>%
  group_by(la_2) %>%
  summarise(bicycle = sum(bicycle),
            foot = sum(foot),
            car_driver = sum(car_driver),
            all = sum(all))
national_dest_la = national_dest_la %>%
  rename(bicycle_d = bicycle,
         foot_d = foot,
         car_driver_d = car_driver,
         all_d = all)

# Mean of the two
national_la = inner_join(national_origin_la, national_dest_la, by = c("la_1" = "la_2"))
national_la = national_la %>%
  mutate(bicycle_mean = (bicycle_o + bicycle_d)/2,
         foot_mean = (foot_o + foot_d)/2,
         car_driver_mean = (car_driver_o + car_driver_d)/2,
         all_mean = (all_o + all_d)/2
         )

# Join to upper tier LAs
lookup_la_lad = readRDS("lookup_la_lad.Rds")
national_la = left_join(national_la, lookup_la_lad, by = c("la_1" = "LAD19NM"))

check = national_la %>%
  filter(is.na(ctyua19nm))
View(check) # many NAs remain from old LAs

# national_desire = get_desire_lines(region = "london", omit_intrazonal = FALSE)
#
# regions = pct::pct_regions
# i = "isle-of-wight"
# region_names_ordered = regions$region_name
#
# for(i in region_names_ordered) {
#   message("building for ", i)
#   des = get_desire_lines(region = i, omit_intrazonal = FALSE)
#   f = paste0("desire-", i, ".Rds")
#   saveRDS(des, f)
# }
