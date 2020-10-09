

library(tidyverse)
library(mgcv)
library(ggplot2)
# library(viridis)
library(mapview)
# theme_set(theme_bw())
library(ggpubr)

# Older traffic data reaching up to 2018
# traffic_national = readRDS("traffic_data_national.Rds")

# la_lookup = traffic_national %>%
#   select(local_authority_id, local_authority_name) %>%
#   distinct()
# saveRDS(la_lookup, "la-lookup.Rds")


##Latest DfT dataset including 2019 counts
# remotes::install_github("itsleeds/dftTrafficCounts")
# library(dftTrafficCounts)
#
# u = "http://data.dft.gov.uk/road-traffic/dft_traffic_counts_raw_counts.zip"
# d = dtc_import(u = u)
#
# saveRDS(d, "traffic-data-29092020.Rds")

traffic_latest = readRDS("traffic-data-29092020.Rds")


# Get local authority names
la_lookup = readRDS("la-lookup.Rds")
traffic_latest = traffic_latest %>%
  inner_join(la_lookup, by = c("LocalAuthority" = "local_authority_id"))

traffic_latest = traffic_latest %>%
  rename(year = Year,
         count_point_id = CP,
         direction_of_travel = iDir,
         pedal_cycles = PedalCycles,
         easting = Easting,
         northing = Northing,
         local_authority_id = LocalAuthority,
         road_category = RoadCategory,
         road_type = RoadType)


traffic_points = traffic_latest %>%
  select(year, count_date, hour, local_authority_name, count_point_id, road_category, easting, northing, pedal_cycles) %>%
  group_by(year, count_date, hour, local_authority_name, count_point_id, road_category, easting, northing) %>%
  summarise(pedal_cycles = sum(pedal_cycles)) %>%
  ungroup()

# remove motorways
traffic_points = traffic_points %>%
  filter(road_category != "TM",
         road_category != "PM")

traffic_bam = transform(traffic_points,
                        count_point_id = factor(count_point_id),
                        local_authority_name = factor(local_authority_name),
                        road_category = factor(road_category),
                        DoY = as.numeric(lubridate::yday(count_date)))


# Fix points with location errors
# traffic_bam %>% filter(count_point_id == 946853) %>%
#   sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>% mapview()
# traffic_bam %>% filter(count_point_id == 952939) %>%
#   sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>% mapview()

error1 = traffic_bam %>%
  filter(count_point_id == 946853)
error2 = traffic_bam %>%
  filter(count_point_id == 952939)

error1[error1$easting == 135809,] = error1[error1$easting == 135809,] %>%
  mutate(northing = 24870)
error2 = error2 %>%
  mutate(northing = 221460)


traffic_bam = traffic_bam %>%
  filter(count_point_id != 952939,
         count_point_id != 946853)
traffic_bam = rbind(traffic_bam, error1, error2)

# Remove counts with hour errors
# traffic_bam %>%
#   filter(hour == 0 | hour == 1 | hour == 3 | hour == 4 | hour == 5)
traffic_bam = traffic_bam %>%
  filter(hour %in% 7:18)


# Assign count points to 2km grid squares # this has to use `round()` instead of `signif()`
traffic_bam = traffic_bam %>%
  mutate(grid_location = paste((round((traffic_bam$easting/2), digits = -3)*2),(round((traffic_bam$northing/2), digits = -3)*2)))

# dim(traffic_bam) #2098824
# length(unique(traffic_bam$grid_location))


#  Investigate count point numbers and placements per/across the year ----------------
traffic_days = traffic_bam %>%
  group_by(year, count_date, DoY, local_authority_name, road_category, count_point_id, easting, northing, grid_location) %>%
  summarise(pedal_cycles = sum(pedal_cycles))

#Get LA boundaries for maps
lads = readRDS("lads.Rds")

# traffic_days %>%
#   sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
#   # filter(year == 2011) %>%
#   mapview() + mapview(lads)


#Identify repeat count point locations
dim(traffic_days) #174902
length(unique(traffic_days$count_point_id))

#filter out any count points that don't appear within both the first and second periods of 2009-2013 and 2014-2019?
early_year = traffic_days %>%
  group_by(count_point_id, year) %>%
  filter(year %in% 2009:2013)
early = unique(early_year$count_point_id)
length(early)

late_year = traffic_days %>%
  group_by(count_point_id, year) %>%
  filter(year %in% 2014:2019)
late = unique(late_year$count_point_id)
length(late)

repeat_points = intersect(early, late)

day_repeats = traffic_days %>%
  filter(count_point_id %in% repeat_points)
dim(traffic_days) #174902
dim(day_repeats) #98343

bam_repeats = traffic_bam %>%
  filter(count_point_id %in% repeat_points)
dim(traffic_bam) #2098824
dim(bam_repeats) #1180116

bam_2010_on = traffic_bam %>%
  filter(year %in% 2010:2019)
dim(bam_2010_on) #986736

#number of count points per year
traffic_days %>%
  group_by(year) %>%
  count()

traffic_bam %>%
  group_by(hour) %>%
  tally()

traffic_bam %>%
  group_by(DoY) %>%
  tally()


# sampling by day of year. It starts slowly in march and goes on to november, very few counts in final weeks. No counts in August.
ggdensity(traffic_days, x = "DoY",
          fill = "#0073C2FF", color = "#0073C2FF",
          rug = TRUE)
gghistogram(traffic_days, x = "DoY", bins = 314,
            fill = "#0073C2FF", color = "#0073C2FF",
            # rug = TRUE,
            xlab = "Day of Year",
            ylab = "Number of cycle counts")

# Get vector of days for which we have at least 10 counts
counts_per_day = traffic_days %>%
  group_by(DoY) %>%
  tally()
days_to_use = counts_per_day %>%
  filter(n >= 40)
days_to_use = days_to_use$DoY

# sampling by year - there were more counts in 2008 and 2009 than other years
gghistogram(traffic_days, x = "year",
            fill = "#0073C2FF", color = "#0073C2FF",
            bins = 20,
            rug = TRUE,
            xlab = "Year",
            ylab = "Number of cycle counts")

# road categories by year
cats = day_repeats %>%
  group_by(year, road_category) %>%
  count()
# extra = data.frame(year = c(2019, 2019), road_category = c("PA", "TA"), n = c(0, 0))
# cats = rbind(cats, extra)
annual_totals = cats %>% group_by(year) %>%
  summarise(n = sum(n)) %>% rename(total = n)
cats = cats %>%
  inner_join(annual_totals, by = "year") %>%
  mutate(proportion = n/total)
 # View(cats)

ggplot(cats,
       aes(x = year, y = n, fill = factor(road_category))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Road category",
                      labels = c("MB", "MCU", "PA", "TA")) +
  xlab("Year") + ylab("Number of counts")

## investigate density of sampling points across UK spatially
grid_density = traffic_days %>%
  group_by(year, grid_location) %>%
  summarise(n = length(unique(count_point_id)))


forplot = traffic_bam %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
plot(pedal_cycles ~ year, data = forplot)


forplot = traffic_bam %>%
  group_by(hour) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
plot(pedal_cycles ~ hour, data = forplot)

forplot = traffic_bam %>%
  group_by(road_category) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
plot(pedal_cycles ~ road_category, data = forplot)




# randomise the row order of the data
set.seed(42)
new_order = sample(nrow(bam_2010_on))
bam_2010_on = bam_2010_on[new_order, ]

# Fit GAM model using national grid locations--------------------------------------------

M = list(c(1, 0.5), NA)

sample_bam = bam_2010_on[1:50000,]

system.time({m4 = bam(pedal_cycles ~
          s(year, bs = "cr", k = 5),
        family = nb(link = "log"),
        data = sample_bam, method = 'fREML',
        nthreads = 4, discrete = TRUE)}) #102 seconds
summary(m4)

plot(m4, pages = 1, scheme = 2, shade = TRUE)

system.time({m2 = bam(pedal_cycles ~
           s(DoY, bs = "cr", k = 3),
         family = nb(link = "log"),
         data = sample_bam, method = 'fREML',
         nthreads = 4, discrete = TRUE)}) #96 seconds
summary(m2)

plot(m2, pages = 1, scheme = 2, shade = TRUE)

system.time({m3 = bam(pedal_cycles ~
           s(hour),
         family = nb(link = "log"),
         data = traffic_bam, method = 'fREML',
         nthreads = 4, discrete = TRUE)}) #90 seconds
summary(m3)

plot(m3, pages = 1, scheme = 2, shade = TRUE)

system.time({m5 = bam(pedal_cycles ~
                        s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5)),
                      family = nb(link = "log"),
                      data = traffic_bam, method = 'fREML',
                      nthreads = 4, discrete = TRUE)}) #135 seconds
summary(m5)
plot(m5, pages = 1, scheme = 2, shade = TRUE, scale = 1)

system.time({m6 = bam(pedal_cycles ~
                        s(year, bs = "cr", k = 3) +
                        s(DoY, bs = "cr", k = 3) +
                        s(hour) +
                        s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5)),

                      family = nb(link = "log"),
                      data = traffic_bam, method = 'fREML',
                      nthreads = 4, discrete = TRUE)}) #101 seconds
summary(m6)
plot(m6, pages = 1, scheme = 2, shade = TRUE)

system.time({m8 = bam(pedal_cycles ~
                        s(year, k = 5) +
                        s(DoY, bs = "cr", k = 3) +
                        s(hour) +
                        s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5)) +
                        ti(easting, northing, year, d = c(2,1), bs = c('ds','cr'),
                           m = M, k = c(25, 3)) +
                        ti(easting, northing, hour, d = c(2,1), bs = c('ds','tp'),
                           m = M, k = c(25, 5)),

                      family = nb(link = "log"),
                      data = sample_bam, method = 'fREML',
                      nthreads = 4, discrete = TRUE)}) #307 seconds
summary(m8)
plot(m8, pages = 1, scheme = 2, shade = TRUE)

sample_bam = sample_bam %>%
  filter(year %in% 2010:2017)

# include road category as a random effect
system.time({m9 = bam(pedal_cycles ~
                        s(year, k = 5) +
                        s(DoY, bs = "cr", k = 3) +
                        s(hour) +
                        s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5)) +
                        s(road_category, bs = "re")
                        #+ ti(easting, northing, year, d = c(2,1), bs = c('ds','cr'),
                        #    m = M, k = c(25, 3)) +
                        # ti(easting, northing, hour, d = c(2,1), bs = c('ds','tp'),
                        #    m = M, k = c(25, 5))
                      ,

                      family = nb(link = "log"),
                      data = sample_bam, method = 'fREML',
                      nthreads = 4, discrete = TRUE)}) #307 seconds
summary(m9)
plot(m9, pages = 1, scheme = 2, shade = TRUE)


########luton
luton %>%
  +     sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  +     # filter(year == 2011) %>%
  +     mapview() + mapview(lads)
luton = luton %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700)
mapview(luton)

lhs = luton %>% filter(count_point_id == 808197)
unique(lhs$year) #2019 only
mapview(lhs)

obr = luton %>% filter(count_point_id == 941693)
unique(obr$year) #
mapview(obr)

