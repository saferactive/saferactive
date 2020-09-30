##Latest DfT dataset including 2019 counts
remotes::install_github("itsleeds/dftTrafficCounts")
library(dftTrafficCounts)
library(tidyverse)
library(mgcv)
library(ggplot2)

u = "http://data.dft.gov.uk/road-traffic/dft_traffic_counts_aadf.zip"
d = dtc_import(u = u)

saveRDS(d, "traffic-aadf-29092020.Rds")
traffic_aadf = readRDS("traffic-aadf-29092020.Rds")

# dim(traffic_aadf) #461948


traffic_points = traffic_aadf %>%
  select(year, local_authority_name, count_point_id, road_category, easting, northing, pedal_cycles, estimation_method, estimation_method_detailed) %>%
  group_by(year, local_authority_name, count_point_id, road_category, easting, northing, estimation_method, estimation_method_detailed) %>%
  summarise(pedal_cycles = sum(pedal_cycles)) %>%
  ungroup()

# remove motorways
traffic_points = traffic_points %>%
  filter(road_category != "TM",
         road_category != "PM")

# remove estimated points
traffic_points = traffic_points %>%
  filter(estimation_method == "Counted")
dim(traffic_points) #183884
# there are some roads with estimation_method_detailed "dependent on a nearby count point". This is eg where a road crosses a county boundary and the same count has been applied to segments either side of this boundary. To get more comprehensive mileage coverage, these points are included

traffic_bam = transform(traffic_points,
                        count_point_id = factor(count_point_id),
                        local_authority_name = factor(local_authority_name),
                        road_category = factor(road_category))


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


# Assign count points to 2km grid squares # this has to use `round()` instead of `signif()`
traffic_bam = traffic_bam %>%
  mutate(grid_location = paste((round((traffic_bam$easting/2), digits = -3)*2),(round((traffic_bam$northing/2), digits = -3)*2)))

# dim(traffic_bam) #183884
# length(unique(traffic_bam$grid_location))



# count points with at least 3 years of counted data 2009-2019
repeat_points = traffic_bam %>%
  filter(year %in% 2010:2019) %>%
  group_by(count_point_id) %>%
  tally() %>%
  filter(n >= 5)
# 9622
traffic_repeats = traffic_bam %>%
  filter(count_point_id %in% repeat_points$count_point_id) %>%
  filter(year %in% 2010:2019)
dim(traffic_repeats) #54620

#filter out any count points that don't appear within both the first and second periods of 2009-2013 and 2014-2019?
early_year = traffic_bam %>%
  group_by(count_point_id, year) %>%
  filter(year %in% 2010:2014)
early = unique(early_year$count_point_id)
length(early)

late_year = traffic_bam %>%
  group_by(count_point_id, year) %>%
  filter(year %in% 2015:2019)
late = unique(late_year$count_point_id)
length(late)

repeat_points = intersect(early, late)

traffic_repeats = traffic_repeats %>%
  filter(count_point_id %in% repeat_points) %>%
  filter(year %in% 2010:2019)
dim(traffic_bam) #183884
dim(traffic_repeats) #68284

#Exploratory analysis
traffic_repeats %>%
  group_by(year, road_category) %>%
  tally() %>%
  View()

forplot = traffic_bam %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
plot(pedal_cycles ~ year, data = forplot)

forplot = traffic_repeats %>%
  # filter(road_category == "TA") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
plot(pedal_cycles ~ year, data = forplot)

#plot by year and road category

## all count points
mb = traffic_bam %>%
  filter(road_category == "MB") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
pa = traffic_bam %>%
  filter(road_category == "PA") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
mcu = traffic_bam %>%
  filter(road_category == "MCU") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
ta = traffic_bam %>%
  filter(road_category == "TA") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))

plot(pedal_cycles ~ year, data = pa, col = "red", ylim = c(0, 350), type = "n")
lines(pedal_cycles ~ year, data = pa, col = "red")
lines(pedal_cycles ~ year, data = mb, col = "blue")
lines(pedal_cycles ~ year, data = mcu, col = "green")
lines(pedal_cycles ~ year, data = ta, col = "yellow")
legend(2000, 350, legend = c("PA", "MB", "MCU", "TA"), col = c("red", "blue", "green", "yellow"), lty = 1)
title(main = "Mean cycle count across all count points")

##counts repeated 3 times 2009-2019
mb = traffic_repeats %>%
  filter(road_category == "MB") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
pa = traffic_repeats %>%
  filter(road_category == "PA") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
mcu = traffic_repeats %>%
  filter(road_category == "MCU") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
ta = traffic_repeats %>%
  filter(road_category == "TA") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))

plot(pedal_cycles ~ year, data = pa, col = "red", ylim = c(0, 500), type = "n")
lines(pedal_cycles ~ year, data = pa, col = "red")
lines(pedal_cycles ~ year, data = mb, col = "blue")
lines(pedal_cycles ~ year, data = mcu, col = "green")
lines(pedal_cycles ~ year, data = ta, col = "yellow")
legend(2010, 500, legend = c("PA", "MB", "MCU", "TA"), col = c("red", "blue", "green", "yellow"), lty = 1)
title(main = "Mean cycle count for points sampled >=5 times 2010 - 2019", cex.main = 0.8)

# road categories by year
cats = traffic_repeats %>%
  group_by(year, road_category) %>%
  count()
# extra = data.frame(year = c(2019, 2019), road_category = c("PA", "TA"), n = c(0, 0))
# cats = rbind(cats, extra)
annual_totals = cats %>% group_by(year) %>%
  summarise(n = sum(n)) %>% rename(total = n)
cats = cats %>%
  inner_join(annual_totals, by = "year") %>%
  mutate(proportion = n/total)
View(cats)

ggplot(cats,
       aes(x = year, y = n, fill = factor(road_category))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_discrete(name = "Road category",
                      labels = c("MB", "MCU", "PA", "TA")) +
  xlab("Year") + ylab("Number of counts")

#gam models
M = list(c(1, 0.5), NA)

# randomise the row order of the data
set.seed(42)
new_order = sample(nrow(traffic_repeats))
traffic_repeats = traffic_repeats[new_order, ]

sample_bam = traffic_repeats[1:50000,]

system.time({m4 = bam(pedal_cycles ~
                        s(year, bs = "cr", k = 5) +
                        s(road_category, bs = "re") +
                        s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5)) +
                        ti(easting, northing, year, d = c(2,1), bs = c('ds','cr'), m = M, k = c(25, 3)),
                        # ti(year, road_category, bs = c("tp", "re")),
                      family = nb(link = "log"),
                      data = traffic_repeats, method = 'fREML',
                      nthreads = 4, discrete = TRUE)}) #102 seconds
summary(m4)
plot(m4, pages = 1, scheme = 2, shade = TRUE)

library(nlme)
system.time({m4 = lme(pedal_cycles ~ year,
                      family = nb(link = "log"),
                      data = sample_bam,
                      random = road_category)}) #102 seconds
summary(m4)
plot(m4, pages = 1, scheme = 2, shade = TRUE)

library(glmmTMB)
system.time({m2 <- glmmTMB(pedal_cycles~year+(1|road_category),
                         data=sample_bam,
                         family=nbinom1)}) #
plot(m2, pages = 1, scheme = 2, shade = TRUE)
