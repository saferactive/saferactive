# The stats in this script are based on peak hour cycle flows and peak hour collisions only, with the aim of approximating commuter cycling. This is because PCT results are for commuting only.

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

piggyback::pb_download("crash_2010_2019_with_summary_adjusted_casualties.Rds")
# crash_raw = readRDS("crash_2010_2019_with_summary_adjusted_casualties.Rds")

# For 2019 LAD names ------------------------------------------------------

# piggyback::pb_download("crash_2010_2019_with_summary_adjusted_casualties.Rds")
crash_raw = readRDS("crash_2010_2019_with_summary_adjusted_casualties.Rds")

crash_raw = st_drop_geometry(crash_raw)

# # Correct location error
# # View(crash_raw %>% filter(local_authority_district == "Dover" & police_force == "Thames Valley"))
# crash_raw$local_authority_district[crash_raw$accident_index == "2018430248885"] = "Windsor and Maidenhead"
# crash_raw$local_authority_highway[crash_raw$accident_index == "2018430248885"] = "Windsor and Maidenhead"
#
# saveRDS(crash_raw, "data/crash_2010_2019_with_summary_adjusted_casualties.Rds")

# Get and apply LAD codes (joining using 2011 LAD names)
# Notice slight change of file name () replaced with "."
la.file = "Local_Authority_Districts_.December_2019._Names_and_Codes_in_the_United_Kingdom_updated.csv"
# piggyback::pb_download(la.file)
stopifnot(file.exists(la.file))
names_lad = read_csv(la.file)

namejoin = left_join(crash_raw, names_lad, by = c("local_authority_district" = "LAD19NM"))


# Check no rows are mising codes
xx = namejoin %>% filter(is.na(LAD19CD)) %>% group_by(local_authority_district) %>% summarise()

# Standardise to 2019 LAD names
la.file.updated = "Local_Authority_Districts_.December_2019._Names_and_Codes_in_the_United_Kingdom.csv"
# piggyback::pb_download(la.file.updated)

stopifnot(file.exists(la.file.updated))
new_names = read_csv(la.file.updated) %>%
  select(-LAD19NMW, -FID)

nameagain = inner_join(new_names, namejoin, by = "LAD19CD")

# Check no rows are mising codes
xx = nameagain %>% filter(is.na(LAD19NM)) %>% group_by(local_authority_district) %>% summarise()
# xx
length(unique(nameagain$local_authority_district)) #380
length(unique(nameagain$LAD19NM)) #367
length(unique(nameagain$LAD19CD)) #367

nameagain$local_authority_district[nameagain$local_authority_district == "St. Albans"] = "St Albans"
nameagain$local_authority_district[nameagain$local_authority_district == "St. Edmundsbury"] = "St Edmundsbury"
nameagain$local_authority_district[nameagain$local_authority_district == "Stratford-upon-Avon"] = "Stratford-on-Avon"

nameagain = nameagain %>%
  rename(LAD11NM = local_authority_district)

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
unique(xx$LAD19NM) # should be null

crash$year = lubridate::year(crash$date)
crash$hour = lubridate::hour(crash$datetime)
crash$la_name = NULL

# these use LAD19NM. I will need to regenerate the data using LAD11NM to make it compatible with the rural_urban LAD classifications
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

saveRDS(pf_lookup, "pf_lookup.Rds")

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

la$ksi_perBkm_2010 = la$ksi_2010 / la$km_cycle_2010 * 1000000
la$ksi_perBkm_2011 = la$ksi_2011 / la$km_cycle_2011 * 1000000
la$ksi_perBkm_2012 = la$ksi_2012 / la$km_cycle_2012 * 1000000
la$ksi_perBkm_2013 = la$ksi_2013 / la$km_cycle_2013 * 1000000
la$ksi_perBkm_2014 = la$ksi_2014 / la$km_cycle_2014 * 1000000
la$ksi_perBkm_2015 = la$ksi_2015 / la$km_cycle_2015 * 1000000
la$ksi_perBkm_2016 = la$ksi_2016 / la$km_cycle_2016 * 1000000
la$ksi_perBkm_2017 = la$ksi_2017 / la$km_cycle_2017 * 1000000
la$ksi_perBkm_2018 = la$ksi_2018 / la$km_cycle_2018 * 1000000
la$ksi_perBkm_2019 = la$ksi_2019 / la$km_cycle_2019 * 1000000

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

la_pf$ksi_perBkm_2010 = la_pf$ksi_2010 / la_pf$km_cycle_2010 * 1000000
la_pf$ksi_perBkm_2011 = la_pf$ksi_2011 / la_pf$km_cycle_2011 * 1000000
la_pf$ksi_perBkm_2012 = la_pf$ksi_2012 / la_pf$km_cycle_2012 * 1000000
la_pf$ksi_perBkm_2013 = la_pf$ksi_2013 / la_pf$km_cycle_2013 * 1000000
la_pf$ksi_perBkm_2014 = la_pf$ksi_2014 / la_pf$km_cycle_2014 * 1000000
la_pf$ksi_perBkm_2015 = la_pf$ksi_2015 / la_pf$km_cycle_2015 * 1000000
la_pf$ksi_perBkm_2016 = la_pf$ksi_2016 / la_pf$km_cycle_2016 * 1000000
la_pf$ksi_perBkm_2017 = la_pf$ksi_2017 / la_pf$km_cycle_2017 * 1000000
la_pf$ksi_perBkm_2018 = la_pf$ksi_2018 / la_pf$km_cycle_2018 * 1000000
la_pf$ksi_perBkm_2019 = la_pf$ksi_2019 / la_pf$km_cycle_2019 * 1000000

saveRDS(la_pf, "cycle-collision-risk-pf.Rds")

# Read in results ---------------------------------------------------------

la = readRDS("cycle-collision-risk.Rds")
la_pf = readRDS("cycle-collision-risk-pf.Rds")

# remove extra columns
# la_cut = la[,c("LAD19NM",names(la)[grepl("ksi_perBkm_",names(la))])]
la = ungroup(la)
la_pf = ungroup(la_pf)

#removes Scottish LAs which we don't have estimated cycle flows for
la = la[!is.na(la$ksi_perBkm_2019),]
la_pf = la_pf[!is.na(la_pf$ksi_perBkm_2019),]
# head(la)


# Get LAD populations -----------------------------------------------------
# from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
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
xx = la %>%
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


# Police force area populations -------------------------------------------
# piggyback::pb_download("pf_lookup.Rds")
pf_lookup = readRDS("pf_lookup.Rds")

pf_pop = left_join(pop2, pf_lookup, by = c("ladcode20" = "la_code"))

# check for NAs
xx = pf_pop %>%
  filter(is.na(LAD19NM))

pf_pop = pf_pop %>%
  group_by(police_force) %>%
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

la_pf = left_join(la_pf, pf_pop, by = "police_force")

# Calculate km_cycled per capita in each year
la_pf = la_pf %>%
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


# Workday population ------------------------------------------------------
# (workplace population plus residents who don't work)
# Repeat analyses using this. km_percap will decrease for inner london and increase for outer london / rural areas
# from https://www.nomisweb.co.uk/census/2011/wd102ew

# piggyback::pb_download("workday-population.csv", tag = "0.1.4")
workday_pop = read_csv("workday-population.csv")

la = left_join(la, workday_pop, by = c("la_code" = "lad_code"))

# check for NAs
xx = la %>%
  filter(is.na(wdpop_2011))

# Calculate km_cycled per workday capita in each year (based on 2011 workday population)
la = la %>%
  mutate(
    km_perwd_2010 = km_cycle_2010 / wdpop_2011,
    km_perwd_2011 = km_cycle_2011 / wdpop_2011,
    km_perwd_2012 = km_cycle_2012 / wdpop_2011,
    km_perwd_2013 = km_cycle_2013 / wdpop_2011,
    km_perwd_2014 = km_cycle_2014 / wdpop_2011,
    km_perwd_2015 = km_cycle_2015 / wdpop_2011,
    km_perwd_2016 = km_cycle_2016 / wdpop_2011,
    km_perwd_2017 = km_cycle_2017 / wdpop_2011,
    km_perwd_2018 = km_cycle_2018 / wdpop_2011,
    km_perwd_2019 = km_cycle_2019 / wdpop_2011
    )

# Calculate rates ---------------------------------------------------------

# mean rates in each LA
la$mean_risk = apply(la[,names(la)[grepl("ksi_perBkm_",names(la))]], 1, mean, na.rm = TRUE)
la$early_risk = apply(la[,names(la)[grepl("ksi_perBkm_",names(la))]][,1:5], 1, mean, na.rm = TRUE)
la$late_risk = apply(la[,names(la)[grepl("ksi_perBkm_",names(la))]][,6:10], 1, mean, na.rm = TRUE)
la$diff_risk = (la$late_risk / la$early_risk -1) * 100
la$mean_km_cycled = apply(la[,names(la)[grepl("km_cycle_",names(la))]], 1, mean, na.rm = TRUE)

la$mean_cycle_ksi = apply(la[,names(la)[grepl("ksi_20",names(la))]], 1, mean, na.rm = TRUE)
la$early_ksi = apply(la[,names(la)[grepl("ksi_20",names(la))]][,1:5], 1, mean, na.rm = TRUE)
la$late_ksi = apply(la[,names(la)[grepl("ksi_20",names(la))]][,6:10], 1, mean, na.rm = TRUE)
la$diff_ksi = (la$late_ksi / la$early_ksi -1) * 100

la$mean_km_percap = apply(la[,names(la)[grepl("km_percap_",names(la))]], 1, mean, na.rm = TRUE)
la$early_km_percap = apply(la[,names(la)[grepl("km_percap_",names(la))]][,1:5], 1, mean, na.rm = TRUE)
la$late_km_percap = apply(la[,names(la)[grepl("km_percap_",names(la))]][,6:10], 1, mean, na.rm = TRUE)
la$diff_cycle = (la$late_km_percap / la$early_km_percap - 1) * 100

la$mean_km_perwd = apply(la[,names(la)[grepl("km_perwd_",names(la))]], 1, mean, na.rm = TRUE)
la$early_km_perwd = apply(la[,names(la)[grepl("km_perwd_",names(la))]][,1:5], 1, mean, na.rm = TRUE)
la$late_km_perwd = apply(la[,names(la)[grepl("km_perwd_",names(la))]][,6:10], 1, mean, na.rm = TRUE)
la$diff_cycle_wd = (la$late_km_perwd / la$early_km_perwd - 1) * 100

# mean rates in each Police Force area
la_pf$mean_risk = apply(la_pf[,names(la_pf)[grepl("ksi_perBkm_",names(la_pf))]], 1, mean, na.rm = TRUE)
la_pf$early_risk = apply(la_pf[,names(la_pf)[grepl("ksi_perBkm_",names(la_pf))]][,1:5], 1, mean, na.rm = TRUE)
la_pf$late_risk = apply(la_pf[,names(la_pf)[grepl("ksi_perBkm_",names(la_pf))]][,6:10], 1, mean, na.rm = TRUE)
la_pf$diff_risk = la_pf$late_risk - la_pf$early_risk
la_pf$mean_km_cycled = apply(la_pf[,names(la_pf)[grepl("km_cycle_",names(la_pf))]], 1, mean, na.rm = TRUE)
la_pf$mean_cycle_ksi = apply(la_pf[,names(la_pf)[grepl("ksi_20",names(la_pf))]], 1, mean, na.rm = TRUE)

la_pf$mean_km_percap = apply(la_pf[,names(la_pf)[grepl("km_percap_",names(la_pf))]], 1, mean, na.rm = TRUE)
la_pf$early_km_percap = apply(la_pf[,names(la_pf)[grepl("km_percap_",names(la_pf))]][,1:5], 1, mean, na.rm = TRUE)
la_pf$late_km_percap = apply(la_pf[,names(la_pf)[grepl("km_percap_",names(la_pf))]][,6:10], 1, mean, na.rm = TRUE)
la_pf$diff_cycle = (la_pf$late_km_percap / la_pf$early_km_percap - 1) * 100

# Get urban rural classification (England only) ---------------------------
# from https://www.gov.uk/government/statistics/2011-rural-urban-classification-of-local-authority-and-other-higher-level-geographies-for-statistical-purposes
# donloaded the ODT file, converted to CSV and
## piggyback::pb_upload("RUC11_LAD11_ENv2.csv")
piggyback::pb_download("RUC11_LAD11_ENv2.csv")
urban_rural = read_csv("RUC11_LAD11_ENv2.csv")

urban_rural$RUC11[urban_rural$RUC11 == "Mainly Rural (rural including hub towns >=80%)"] = "Mainly Rural"
urban_rural$RUC11[urban_rural$RUC11 == "Largely Rural (rural including hub towns 50-79%)"] = "Largely Rural"
urban_rural$RUC11[urban_rural$RUC11 == "Urban with Significant Rural (rural including hub towns 26-49%)"] = "Urban with Significant Rural"

unique(urban_rural$RUC11)

la_urb = left_join(la, urban_rural, by = c("la_code" = "LAD11CD"))
la_urb$RUC11 = factor(la_urb$RUC11, levels = unique(la_urb$RUC11[order(la_urb$RUC11CD)]))

# Many are missing due to LA changes, need to fix this
xx = la_urb %>% filter(is.na(RUC11CD))

# Boxplots of urban rural classification

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

la_urb %>%
  group_by(RUC11) %>%
  mutate(outlier = ifelse(is_outlier(mean_risk), LAD19NM, as.character(NA))) %>%
  ggplot(., aes(x=factor(RUC11), y=mean_risk
                , weight = population_2011
                )) +
  geom_boxplot(notch = TRUE
               , aes(weight = population_2011)
               ) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(x = "", y = "Mean cycle KSI per Bkm cycled") +
  ylim(0, 3000)
  # + geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.2, cex = 2.5)
table(la_urb$RUC11)

# boxplot of km per cap residential popn
b = la_urb %>%
    filter(! LAD19NM == "City of London") %>%
    group_by(RUC11) %>%
    mutate(outlier = ifelse(is_outlier(mean_risk), LAD19NM, as.character(NA))) %>%
    ggplot(., aes(x=factor(RUC11), y=mean_km_percap)) +
    geom_boxplot(notch = TRUE) +
    theme(axis.text.x=element_text(angle=45,hjust=1)) +
    labs(x = "", y = "Km (1000s) per cap")
b + scale_y_continuous(trans="log10")

  # boxplot of km per cap workday popn
b = la_urb %>%
    filter(! LAD19NM == "City of London") %>%
    group_by(RUC11) %>%
    mutate(outlier = ifelse(is_outlier(mean_risk), LAD19NM, as.character(NA))) %>%
    ggplot(., aes(x=factor(RUC11), y=mean_km_percap)) +
    geom_boxplot(notch = TRUE) +
    theme(axis.text.x=element_text(angle=45,hjust=1)) +
    labs(x = "", y = "Km (1000s) per cap")
b + scale_y_continuous(trans="log10")

# Look at major conurbations only

maj_con = la_urb %>%
  filter(RUC11CD == 6)

ggplot(maj_con, aes(x = mean_km_percap, y = mean_risk)) +
  geom_point() +
  xlim(0, 0.3) +
  ylim(0, 3000) +
  labs(x = "Mean km (1000s) cycled per capita (resident pop'n)", y = "Mean cycle KSI per Bkm cycled") +
  geom_text(label = maj_con$LAD19NM, nudge_y = +0.1, cex = 2.5)

# Generate figures --------------------------------------------------------

# select interesting LAs
top_la = unique(c(
  top_n(la, 4, mean_risk)$LAD19NM, # greatest mean casualty rate
  top_n(la, -4, mean_risk)$LAD19NM, # lowest mean casualty rate
  top_n(la, 4, diff_risk)$LAD19NM, # greatest increase in casualty rate from early years to late years
  top_n(la, -4, diff_risk)$LAD19NM # greatest decrease in casualty rate from early years to late years
  ))

top_la_pf = unique(c(
  top_n(la_pf, 4, mean_risk)$police_force, # greatest mean casualty rate
  top_n(la_pf, -4, mean_risk)$police_force, # lowest mean casualty rate
  top_n(la_pf, 4, diff_risk)$police_force, # greatest increase in casualty rate from early years to late years
  top_n(la_pf, -4, diff_risk)$police_force # greatest decrease in casualty rate from early years to late years
))


la_long = pivot_longer(la,
                        cols = starts_with("ksi_perBkm_"),
                        names_prefix = "ksi_perBkm_",
                        names_to = "year",
                        values_to = "ksi_perBkm"
)

la_pf_long = pivot_longer(la_pf,
                       cols = starts_with("ksi_perBkm_"),
                       names_prefix = "ksi_perBkm_",
                       names_to = "year",
                       values_to = "ksi_perBkm"
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

ggplot(la_long, aes(year, ksi_perBkm, colour = LAD19NM_plot, group = LAD19NM)) +
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

ggplot(la_pf_long, aes(year, ksi_perBkm, colour = police_force_plot, group = police_force)) +
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
ggplot(la_urb, aes(x = mean_km_cycled, y = mean_cycle_ksi, group = RUC11)) +
  geom_point(aes(color = RUC11, shape = RUC11)) +
  # geom_text(label = la$LAD19NM, nudge_y = -1.5, cex = 2.8) +
  labs(x = "Mean km (1000s) cycled", y = "Mean cycle KSI")

ggplot(la, aes(x = mean_km_cycled, y = mean_risk)) +
  geom_point()

# could do separate inverse exponential trend lines for each group. if these lines differ, it shows urban/rural classification is a factor
# a caveat - the more rural the area, the higher proportion of leisure cycling there is likely to be. this could be skewing our results, but if it's true it means rural areas are even safer than this analysis suggests
lau = la_urb %>% filter(! LAD19NM == "City of London")
g = ggplot(la_urb %>% filter(! LAD19NM == "City of London"), aes(x = mean_km_percap, y = mean_risk, group = RUC11)) + #can be percap or perwd
  geom_point(aes(color = RUC11, shape = RUC11)) +
  # xlim(0, 0.2) +
  # ylim(0, 3000) +
  theme(legend.title = element_blank()) +
  labs(x = "Mean km (1000s) cycled per capita (resident pop'n)", y = "Mean cycle KSI per Bkm cycled") # +
   # geom_text(label = lau$LAD19NM)
g + scale_x_continuous(trans="log10") +
  geom_smooth(method = lm, aes(colour = RUC11), alpha = 0.2)

#same with workday population
g = ggplot(la_urb, aes(x = mean_km_perwd, y = mean_risk, group = RUC11)) + #can be percap or perwd
  geom_point(aes(color = RUC11, shape = RUC11)) +
  xlim(0, 0.2) +
  ylim(0, 3000) +
  theme(legend.title = element_blank()) +
  labs(x = "Mean km (1000s) cycled per capita (workday pop'n)", y = "Mean cycle KSI per Bkm cycled") # +
# geom_text(label = lau$LAD19NM)
g + scale_x_continuous(trans="log10") +
  geom_smooth(method = lm, aes(colour = RUC11), alpha = 0.2)

# Change in cycling uptake and risk
# this should probably use absolute change in KSI, or change in KSI per capita, not change in KSI/Bkm, because the latter is partly dependent on our estimates of cycle uptake
ggplot(la_urb%>% filter(! LAD19NM == "City of London"), aes(x = diff_cycle, y = diff_risk, group = RUC11CD)) +
  geom_point(aes(color = RUC11, shape = RUC11)) +
  # ylim(-2, 1) +
  # xlim(-0.01, 0.01) +
  theme(legend.title = element_blank()) +
  labs(x = "Modelled % change in cycling uptake", y = "% change in road safety risk") +
  geom_smooth(method = lm, aes(colour = RUC11), alpha = 0.2)
  # + geom_text(label = la$LAD19NM, nudge_y = -0.1, cex = 3)

# change in absolute KSI
ggplot(la %>% filter(! LAD19NM == "City of London"), aes(x = diff_cycle, y = diff_ksi)) +
  geom_point() +
  theme(legend.title = element_blank()) +
  labs(x = "Modelled % change in cycling uptake", y = "% change in absolute cycle KSI") +
  geom_smooth(method = lm, alpha = 0.2)

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
bounds = bounds[!is.na(bounds$ksi_perBkm_2019),]
# bounds10 = bounds[is.na(bounds$ksi_perBkm_2010),]

# Police force geometry
# from https://data.gov.uk/dataset/41f748c0-48d6-48dd-a452-c73c2afae187/police-force-areas-december-2018-ew-buc
## piggyback::pb_upload("Police_Force_Areas_December_2018_EW_BUC.geojson")
piggyback::pb_download("Police_Force_Areas_.December_2018._EW_BUC.geojson")
pf_geom = read_sf("Police_Force_Areas_.December_2018._EW_BUC.geojson") # todo: make reproducible
pf_geom$pfa18nm[pf_geom$pfa18nm == "Devon & Cornwall"] = "Devon and Cornwall"
pf_geom$pfa18nm[pf_geom$pfa18nm == "London, City of"] = "City of London"
pf_geom = left_join(pf_geom, la_pf, by = c("pfa18nm" = "police_force"))
# pf_geom = pf_geom[!is.na(pf_geom$ksi_perBkm_2019),]

tm_shape(bounds) +
  tm_fill("ksi_perBkm_2019") +
  tm_borders(lwd = 0.1) +
  tm_layout(legend.outside = TRUE)

tmap_options(check.and.fix = TRUE)
tm_shape(pf_geom) +
  tm_fill("ksi_perBkm_2019", breaks = c(0, 400, 800, 1200, 1600, 2000)) +
  tm_borders(lwd = 0.1) +
  tm_layout(legend.outside = TRUE)

####
tmap_options(check.and.fix = TRUE)
t1 = tm_shape(bounds) +
  tm_fill("mean_risk", breaks = c(0, 500, 1000, 1500, 2000, 3000, 4000), title = "2010-19") +
  tm_borders(lwd = 0.1)
  # + tm_layout(title = "Cycle KSI/Bkm 2010-19")

t2 = tm_shape(bounds) +
  tm_fill("early_risk", breaks = c(0, 500, 1000, 1500, 2000, 3000, 4000), title = "2010-14") +
  tm_borders(lwd = 0.1)
 # + tm_layout(title = "Cycle KSI/Bkm 2010-14")

t3 = tm_shape(bounds) +
  tm_fill("late_risk", breaks = c(0, 500, 1000, 1500, 2000, 3000, 4000), title = "2015-19") +
  tm_borders(lwd = 0.1)
  # + tm_layout(title = "Cycle KSI/Bkm 2015-19")

tmap_arrange(t1, t2, t3)

####
t1 = tm_shape(pf_geom) +
  tm_fill("mean_risk", breaks = c(0, 400, 800, 1200, 1600, 2000), title = "2010-19") +
  tm_borders(lwd = 0.1)
# + tm_layout(title = "Cycle KSI/Bkm 2010-19")

t2 = tm_shape(pf_geom) +
  tm_fill("early_risk", breaks = c(0, 400, 800, 1200, 1600, 2000), title = "2010-14") +
  tm_borders(lwd = 0.1)
# + tm_layout(title = "Cycle KSI/Bkm 2010-14")

t3 = tm_shape(pf_geom) +
  tm_fill("late_risk", breaks = c(0, 400, 800, 1200, 1600, 2000), title = "2015-19") +
  tm_borders(lwd = 0.1)
# + tm_layout(title = "Cycle KSI/Bkm 2015-19")

tmap_arrange(t1, t2, t3)

# Map change in cycle uptake and risk

bounds = bounds %>% mutate(
  change = case_when(
    diff_cycle > 0 & diff_risk <= 0 ~ "1. more uptake, less risk",
    diff_cycle <= 0 & diff_risk <= 0 ~ "2. less uptake, less risk",
    diff_cycle > 0 & diff_risk > 0 ~ "3. more uptake, more risk",
    diff_cycle <= 0 & diff_risk > 0 ~ "4. less uptake, more risk"
    )
)

tm_shape(bounds) +
  tm_fill("change")


# piggyback::pb_download("la_upper_for_plots.Rds") #not found
# bounds_upper = readRDS("la_upper_for_plots.Rds")

## Histogram of LA values

hist(bounds$mean_risk, xlim = c(0, 4), breaks = 15)
hist(bounds$early_risk, xlim = c(0, 4), breaks = 20)
hist(bounds$late_risk, xlim = c(0, 4), breaks = 15)

# top 10 lists
highest_mean = arrange(top_n(la, 10, mean_risk),-mean_risk) # greatest mean casualty rate

lowest_mean = arrange(top_n(la, 10, -mean_risk),mean_risk) # lowest mean casualty rate

greatest_increase = arrange(top_n(la, 10, diff_risk),-diff_risk) # greatest increase in casualty rate

greatest_decrease = arrange(top_n(la, 10, -diff_risk),diff_risk) # greatest decrease in casualty rate


# Create clean object for TGVE
bounds_clean = bounds %>%
  select(lad_name, mean_risk, diff_risk, diff_cycle_wd, la_code) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  sf::st_drop_geometry()

readr::write_csv(bounds_clean, "../tgve/ksi.csv")

