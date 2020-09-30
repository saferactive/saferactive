##Latest DfT dataset including 2019 counts
remotes::install_github("itsleeds/dftTrafficCounts")
library(dftTrafficCounts)
library(tidyverse)
library(mgcv)
library(ggplot2)

u = "http://data.dft.gov.uk/road-traffic/dft_traffic_counts_aadf.zip"
d = dtc_import(u = u)

saveRDS(d, "traffic-aadf-29092020.Rds")
piggyback::pb_upload("traffic-aadf-29092020.Rds")
traffic_aadf = readRDS("traffic-aadf-29092020.Rds")

dim(traffic_aadf)
# [1] 461948     33

names(traffic_aadf)
table(traffic_aadf$sequence)


traffic_points = traffic_aadf %>%
  select(year, local_authority_name, count_point_id, road_category, easting, northing, pedal_cycles, estimation_method, estimation_method_detailed, link_length_km) %>%
  group_by(year, local_authority_name, count_point_id, road_category, easting, northing, estimation_method, estimation_method_detailed, link_length_km) %>%
  summarise(pedal_cycles = sum(pedal_cycles)) %>%
  ungroup()
nrow(traffic_points) / nrow(traffic_aadf)
# [1] 0.3980621 # why are there multiple reading for each count point?
skimr::skim(traffic_aadf)

traffic_points_sequence = traffic_aadf %>%
  select(year, local_authority_name, count_point_id, road_category, easting, northing, pedal_cycles, estimation_method, estimation_method_detailed, link_length_km, sequence) %>%
  group_by(year, local_authority_name, count_point_id, road_category, easting, northing, estimation_method, estimation_method_detailed, link_length_km, sequence) %>%
  summarise(pedal_cycles = sum(pedal_cycles)) %>%
  ungroup()
nrow(traffic_points_sequence) / nrow(traffic_aadf) # including sequence gives 1:1 fit - indicating repeated counts
# [1] 1

# Next step: explore how dependent the data are on sequences
traffic_sequence = traffic_aadf %>%
  group_by(count_point_id) %>%
  summarise(pedal_cycles = sum(pedal_cycles), n_sequence = length(unique(sequence)))

summary(traffic_sequence$n_sequence)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1.000   1.000   1.000   1.118   1.000   3.000
# what does sequence mean?
cor(traffic_aadf$year, as.numeric(traffic_aadf$sequence), use = "complete.obs")
# [1] -0.05876427 # no relationship with year...

traffic_sequence2 = traffic_aadf %>%
  group_by(count_point_id) %>%
  mutate(mean_pedal_cycles = mean(pedal_cycles), n_sequence = length(unique(sequence))) %>%
  filter(n_sequence > 1) %>%
  group_by(count_point_id, sequence) %>%
  summarise(pedal_cycles_sequence = mean(pedal_cycles))
traffic_sequence2
# # A tibble: 10,600 x 3
# # Groups:   count_point_id [5,298]
# count_point_id sequence pedal_cycles_sequence
# <dbl> <chr>                    <dbl>
#   1            627 3040                     3.22
# 2            627 5040                     5.5
# 3            664 10                      44.8
# 4            664 400                     36
# 5            703 3120                     0
# 6            703 5120                     0
# 7           1187 3210                     0.944
# 8           1187 5210                     0
# 9           1189 3110                     4.67
# 10           1189 5110                     4
# # … with 10,590 more rows
traffic_sequence2 %>%
  summarise(unique_per_sequence = length(unique(pedal_cycles_sequence)))

# remove motorways
traffic_points = traffic_points %>%
  filter(road_category != "TM",
         road_category != "PM")

# remove estimated points
traffic_points = traffic_points %>%
  filter(estimation_method == "Counted")
dim(traffic_points) #183884
# there are some roads with estimation_method_detailed "dependent on a nearby count point". This is where a road crosses a county boundary and the same count has been applied to segments either side of this boundary. These points are included.

# exploratory data analysis



traffic_bam = transform(traffic_points,
                        count_point_id = factor(count_point_id),
                        local_authority_name = factor(local_authority_name),
                        road_category = factor(road_category),
                        link_length_km = as.numeric(link_length_km))



# Link lengths ------------------------------------------------------------
library(readODS)
#Get lengths of roads of each category in each LA
piggyback::pb_download("rdl0202.ods")

colty = c(rep("character", 3), rep("numeric", 8))

road_lengths_2019 = read_ods("rdl0202.ods", sheet = 1, skip = 6, col_types = colty) %>%
  select(c(1:3,9:12,15:18))
road_lengths_2019[,10] = as.numeric(road_lengths_2019[,10])
road_lengths_2019 = road_lengths_2019 %>%
  mutate(TA = .[[4]] + .[[5]],
         PA = .[[6]] + .[[7]],
         MB = .[[8]] + .[[9]],
         MCU = .[[10]] + .[[11]])

road_lengths_2018 = read_ods("rdl0202.ods", sheet = 3, skip = 6, col_types = c()) %>%
  select(c(1:3,9:12,15:18))
road_lengths_2018[,4] = as.numeric(road_lengths_2018[,4])
road_lengths_2018[,5] = as.numeric(road_lengths_2018[,5])
road_lengths_2018[,6] = as.numeric(road_lengths_2018[,6])
road_lengths_2018[,7] = as.numeric(road_lengths_2018[,7])
road_lengths_2018[,8] = as.numeric(road_lengths_2018[,8])
road_lengths_2018[,9] = as.numeric(road_lengths_2018[,9])
road_lengths_2018[,10] = as.numeric(road_lengths_2018[,10])
road_lengths_2018[,11] = as.numeric(road_lengths_2018[,11])
road_lengths_2018 = road_lengths_2018 %>%
  mutate(TA = .[[4]] + .[[5]],
         PA = .[[6]] + .[[7]],
         MB = .[[8]] + .[[9]],
         MCU = .[[10]] + .[[11]])

road_lengths_2017 = read_ods("rdl0202.ods", sheet = 5, skip = 6) %>%
  select(c(1:3,9:12,15:18))
road_lengths_2017[,4] = as.numeric(road_lengths_2017[,4])
road_lengths_2017[,5] = as.numeric(road_lengths_2017[,5])
road_lengths_2017[,6] = as.numeric(road_lengths_2017[,6])
road_lengths_2017[,7] = as.numeric(road_lengths_2017[,7])
road_lengths_2017[,8] = as.numeric(road_lengths_2017[,8])
road_lengths_2017[,9] = as.numeric(road_lengths_2017[,9])
road_lengths_2017[,10] = as.numeric(road_lengths_2017[,10])
road_lengths_2017[,11] = as.numeric(road_lengths_2017[,11])
road_lengths_2017 = road_lengths_2017 %>%
  mutate(TA = .[[4]] + .[[5]],
         PA = .[[6]] + .[[7]],
         MB = .[[8]] + .[[9]],
         MCU = .[[10]] + .[[11]])

road_lengths_2016 = read_ods("rdl0202.ods", sheet = 7, skip = 6) %>%
  select(c(1:3,9:12,15:18))
road_lengths_2016[,4] = as.numeric(road_lengths_2016[,4])
road_lengths_2016[,5] = as.numeric(road_lengths_2016[,5])
road_lengths_2016[,6] = as.numeric(road_lengths_2016[,6])
road_lengths_2016[,7] = as.numeric(road_lengths_2016[,7])
road_lengths_2016[,8] = as.numeric(road_lengths_2016[,8])
road_lengths_2016[,9] = as.numeric(road_lengths_2016[,9])
road_lengths_2016[,10] = as.numeric(road_lengths_2016[,10])
road_lengths_2016[,11] = as.numeric(road_lengths_2016[,11])
road_lengths_2016 = road_lengths_2016 %>%
  mutate(TA = .[[4]] + .[[5]],
         PA = .[[6]] + .[[7]],
         MB = .[[8]] + .[[9]],
         MCU = .[[10]] + .[[11]])

road_lengths_2015 = read_ods("rdl0202.ods", sheet = 9, skip = 6) %>%
  select(c(1:3,9:12,15:18))
road_lengths_2015[,4] = as.numeric(road_lengths_2015[,4])
road_lengths_2015[,5] = as.numeric(road_lengths_2015[,5])
road_lengths_2015[,6] = as.numeric(road_lengths_2015[,6])
road_lengths_2015[,7] = as.numeric(road_lengths_2015[,7])
road_lengths_2015[,8] = as.numeric(road_lengths_2015[,8])
road_lengths_2015[,9] = as.numeric(road_lengths_2015[,9])
road_lengths_2015[,10] = as.numeric(road_lengths_2015[,10])
road_lengths_2015[,11] = as.numeric(road_lengths_2015[,11])
road_lengths_2015 = road_lengths_2015 %>%
  mutate(TA = .[[4]] + .[[5]],
         PA = .[[6]] + .[[7]],
         MB = .[[8]] + .[[9]],
         MCU = .[[10]] + .[[11]])

road_lengths_2014 = read_ods("rdl0202.ods", sheet = 11, skip = 6) %>%
  select(c(1:3,9:12,15:18))
road_lengths_2014[,4] = as.numeric(road_lengths_2014[,4])
road_lengths_2014[,5] = as.numeric(road_lengths_2014[,5])
road_lengths_2014[,6] = as.numeric(road_lengths_2014[,6])
road_lengths_2014[,7] = as.numeric(road_lengths_2014[,7])
road_lengths_2014[,8] = as.numeric(road_lengths_2014[,8])
road_lengths_2014[,9] = as.numeric(road_lengths_2014[,9])
road_lengths_2014[,10] = as.numeric(road_lengths_2014[,10])
road_lengths_2014[,11] = as.numeric(road_lengths_2014[,11])
road_lengths_2014 = road_lengths_2014 %>%
  mutate(TA = .[[4]] + .[[5]],
         PA = .[[6]] + .[[7]],
         MB = .[[8]] + .[[9]],
         MCU = .[[10]] + .[[11]])

road_lengths_2013 = read_ods("rdl0202.ods", sheet = 13, skip = 6) %>%
  select(c(1:3,9:12,15:18))
road_lengths_2013[,4] = as.numeric(road_lengths_2013[,4])
road_lengths_2013[,5] = as.numeric(road_lengths_2013[,5])
road_lengths_2013[,6] = as.numeric(road_lengths_2013[,6])
road_lengths_2013[,7] = as.numeric(road_lengths_2013[,7])
road_lengths_2013[,8] = as.numeric(road_lengths_2013[,8])
road_lengths_2013[,9] = as.numeric(road_lengths_2013[,9])
road_lengths_2013[,10] = as.numeric(road_lengths_2013[,10])
road_lengths_2013[,11] = as.numeric(road_lengths_2013[,11])
road_lengths_2013 = road_lengths_2013 %>%
  mutate(TA = .[[4]] + .[[5]],
         PA = .[[6]] + .[[7]],
         MB = .[[8]] + .[[9]],
         MCU = .[[10]] + .[[11]])

road_lengths_2012 = read_ods("rdl0202.ods", sheet = 15, skip = 6) %>%
  select(c(1:3,9:12,15:18))
road_lengths_2012[,4] = as.numeric(road_lengths_2012[,4])
road_lengths_2012[,5] = as.numeric(road_lengths_2012[,5])
road_lengths_2012[,6] = as.numeric(road_lengths_2012[,6])
road_lengths_2012[,7] = as.numeric(road_lengths_2012[,7])
road_lengths_2012[,8] = as.numeric(road_lengths_2012[,8])
road_lengths_2012[,9] = as.numeric(road_lengths_2012[,9])
road_lengths_2012[,10] = as.numeric(road_lengths_2012[,10])
road_lengths_2012[,11] = as.numeric(road_lengths_2012[,11])
road_lengths_2012 = road_lengths_2012 %>%
  mutate(TA = .[[4]] + .[[5]],
         PA = .[[6]] + .[[7]],
         MB = .[[8]] + .[[9]],
         MCU = .[[10]] + .[[11]])

road_lengths_2011 = read_ods("rdl0202.ods", sheet = 17, skip = 6) %>%
  select(c(1:3,9:12,15:18))
road_lengths_2011[,4] = as.numeric(road_lengths_2011[,4])
road_lengths_2011[,5] = as.numeric(road_lengths_2011[,5])
road_lengths_2011[,6] = as.numeric(road_lengths_2011[,6])
road_lengths_2011[,7] = as.numeric(road_lengths_2011[,7])
road_lengths_2011[,8] = as.numeric(road_lengths_2011[,8])
road_lengths_2011[,9] = as.numeric(road_lengths_2011[,9])
road_lengths_2011[,10] = as.numeric(road_lengths_2011[,10])
road_lengths_2011[,11] = as.numeric(road_lengths_2011[,11])
road_lengths_2011 = road_lengths_2011 %>%
  mutate(TA = .[[4]] + .[[5]],
         PA = .[[6]] + .[[7]],
         MB = .[[8]] + .[[9]],
         MCU = .[[10]] + .[[11]])

road_lengths_2010 = read_ods("rdl0202.ods", sheet = 19, skip = 6) %>%
  select(c(1:3,9:12,15:18))
road_lengths_2010[,4] = as.numeric(road_lengths_2010[,4])
road_lengths_2010[,5] = as.numeric(road_lengths_2010[,5])
road_lengths_2010[,6] = as.numeric(road_lengths_2010[,6])
road_lengths_2010[,7] = as.numeric(road_lengths_2010[,7])
road_lengths_2010[,8] = as.numeric(road_lengths_2010[,8])
road_lengths_2010[,9] = as.numeric(road_lengths_2010[,9])
road_lengths_2010[,10] = as.numeric(road_lengths_2010[,10])
road_lengths_2010[,11] = as.numeric(road_lengths_2010[,11])
road_lengths_2010 = road_lengths_2010 %>%
  mutate(TA = .[[4]] + .[[5]],
         PA = .[[6]] + .[[7]],
         MB = .[[8]] + .[[9]],
         MCU = .[[10]] + .[[11]])

# traffic_bam = traffic_bam %>%
  # filter(! is.na(link_length_km))

# multiple count by link length
traffic_bam = traffic_bam %>%
  mutate(count_times_length = pedal_cycles*link_length_km)



# for major roads, multiply link length by length of roads of the same type within the LA, divided total link lengths for that road type and LA in the dataset


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
