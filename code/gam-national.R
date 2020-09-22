

library(tidyverse)
library(mgcv)
library(ggplot2)
# library(viridis)
library(mapview)
# theme_set(theme_bw())
library(ggpubr)

traffic_national = readRDS("traffic_data_national.Rds")


traffic_points = traffic_national %>%
  select(year, count_date, hour, local_authority_name, count_point_id, easting, northing, pedal_cycles) %>%
  group_by(year, count_date, hour, local_authority_name, count_point_id, easting, northing) %>%
  summarise(pedal_cycles = sum(pedal_cycles)) %>%
  ungroup()

traffic_bam = transform(traffic_points,
                               count_point_id = factor(count_point_id),
                               local_authority_name = factor(local_authority_name),
                               DoY = as.numeric(lubridate::yday(count_date)))


# Fix points with location errors
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

# dim(traffic_bam)
# length(unique(traffic_bam$grid_location))


#  Investigate count point numbers and placements per/across the year ----------------
traffic_days = traffic_bam %>%
  group_by(year, count_date, DoY, local_authority_name, count_point_id, easting, northing, grid_location) %>%
  summarise(pedal_cycles = sum(pedal_cycles))

#Get LA boundaries for maps
lads = readRDS("lads.Rds")

traffic_days %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  # filter(year == 2011) %>%
  mapview() + mapview(lads)

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
            bins = 19,
            rug = TRUE,
            xlab = "Year",
            ylab = "Number of cycle counts")


## plot density of sampling points across london spatially
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



# Fit GAM model using national grid locations--------------------------------------------

M = list(c(1, 0.5), NA)

system.time({m4 = bam(pedal_cycles ~
          s(year, bs = "cr", k = 3),
        family = nb(link = "log"),
        data = traffic_bam, method = 'fREML',
        nthreads = 4, discrete = TRUE)}) #102 seconds
summary(m4)

plot(m4, pages = 1, scheme = 2, shade = TRUE)

system.time({m2 = bam(pedal_cycles ~
           s(DoY, bs = "cr", k = 3),
         family = nb(link = "log"),
         data = traffic_bam, method = 'fREML',
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
