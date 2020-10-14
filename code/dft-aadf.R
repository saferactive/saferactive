# Takes input from `geographic-data-cleaning.R`

##Latest DfT dataset including 2019 counts
library(tidyverse)
library(mgcv)
library(ggplot2)


traffic_cyclable = readRDS("traffic_cyclable_clean.Rds")
dim(traffic_cyclable)
# [1] 183884     35

traffic_points = traffic_cyclable %>%
  select(year, name, count_point_id, road_category, easting, northing, pedal_cycles, estimation_method, estimation_method_detailed, link_length_km) %>%
  group_by(year, name, count_point_id, road_category, easting, northing, estimation_method, estimation_method_detailed, link_length_km) %>%
  summarise(pedal_cycles = sum(pedal_cycles)) %>%
  ungroup()
# nrow(traffic_points) / nrow(traffic_cyclable)
# # [1] 0.3980621 # why are there multiple reading for each count point?
# skimr::skim(traffic_cyclable)

traffic_bam = transform(traffic_points,
                        count_point_id = factor(count_point_id),
                        name = factor(name),
                        road_category = factor(road_category),
                        link_length_km = as.numeric(link_length_km))

# Assign count points to 2km grid squares # this has to use `round()` instead of `signif()`
traffic_bam = traffic_bam %>%
  mutate(grid_location = paste((round((traffic_bam$easting/2), digits = -3)*2),(round((traffic_bam$northing/2), digits = -3)*2)))

# dim(traffic_bam) #183884
# length(unique(traffic_bam$grid_location))

# Remove pre-2010 counts and count points with only 1 year of data --------

repeat_points = traffic_bam %>%
  filter(year %in% 2010:2019) %>%
  group_by(count_point_id) %>%
  tally() %>%
  filter(n > 1)
#13303
traffic_bam = traffic_bam %>%
  filter(count_point_id %in% repeat_points$count_point_id) %>%
  filter(year %in% 2010:2019)
dim(traffic_bam) #69971

##surveyed in 2011
repeat_points = traffic_bam %>%
  filter(year == 2011) %>%
  group_by(count_point_id) %>%
  tally()
traffic_bam = traffic_bam %>%
  filter(count_point_id %in% repeat_points$count_point_id) %>%
  filter(year %in% 2010:2019)
dim(traffic_bam) #54444


# Get relative change in cycle counts  ------------------------------------

traffic_bam = traffic_bam %>%
  group_by(count_point_id) %>%
  mutate(mean_cycles = mean(pedal_cycles)) %>%
  ungroup() %>%
  mutate(change_cycles = pedal_cycles/mean_cycles)
traffic_bam$change_cycles[is.na(traffic_bam$change_cycles)] = 0
sum(is.na(traffic_bam$change_cycles))/nrow(traffic_bam) #0.012
# nas = traffic_bam %>%
#   filter(is.na(change_cycles))
# View(nas)

traffic_bam %>%
  group_by(year) %>%
  summarise(change_cycles = weighted.mean(change_cycles, w = mean_cycles)) %>%
  ggplot(., aes(x = year, y = change_cycles)) +
  geom_line()

traffic_bam %>%
  group_by(year, name) %>%
  summarise(change_cycles = weighted.mean(change_cycles, w = mean_cycles),
            mean_cycles = mean(mean_cycles)) %>%
  ggplot(., aes(x = year, y = change_cycles, group = name)) +
  geom_line(aes(alpha = mean_cycles/100)) +
  ylim(c(0,2))

las_of_interest = c("Leeds", "Derby", "Southampton",
                    "Nottingham", "Birmingham")


# Get London Borough boundaries -------------------------------------------

lads = readRDS("lads.Rds")
boroughs = as.character(spData::lnd$NAME)
lads = lads %>%
  filter(Name %in% boroughs)

# traffic_points_sequence = traffic_cyclable %>%
#   select(year, name, count_point_id, road_category, easting, northing, pedal_cycles, estimation_method, estimation_method_detailed, link_length_km, sequence) %>%
#   group_by(year, name, count_point_id, road_category, easting, northing, estimation_method, estimation_method_detailed, link_length_km, sequence) %>%
#   summarise(pedal_cycles = sum(pedal_cycles)) %>%
#   ungroup()
# nrow(traffic_points_sequence) / nrow(traffic_cyclable) # including sequence gives 1:1 fit - indicating repeated counts
# # [1] 1
#
# # Next step: explore how dependent the data are on sequences
# traffic_sequence = traffic_cyclable %>%
#   group_by(count_point_id) %>%
#   summarise(pedal_cycles = sum(pedal_cycles), n_sequence = length(unique(sequence)))
#
# summary(traffic_sequence$n_sequence)
# # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# # 1.000   1.000   1.000   1.118   1.000   3.000
# # what does sequence mean?
# cor(traffic_cyclable$year, as.numeric(traffic_cyclable$sequence), use = "complete.obs")
# # [1] -0.05876427 # no relationship with year...
#
# traffic_sequence2 = traffic_cyclable %>%
#   group_by(count_point_id) %>%
#   mutate(mean_pedal_cycles = mean(pedal_cycles), n_sequence = length(unique(sequence))) %>%
#   filter(n_sequence > 1) %>%
#   group_by(count_point_id, sequence) %>%
#   summarise(pedal_cycles_sequence = mean(pedal_cycles))
# traffic_sequence2
# # # A tibble: 10,600 x 3
# # # Groups:   count_point_id [5,298]
# # count_point_id sequence pedal_cycles_sequence
# # <dbl> <chr>                    <dbl>
# #   1            627 3040                     3.22
# # 2            627 5040                     5.5
# # 3            664 10                      44.8
# # 4            664 400                     36
# # 5            703 3120                     0
# # 6            703 5120                     0
# # 7           1187 3210                     0.944
# # 8           1187 5210                     0
# # 9           1189 3110                     4.67
# # 10           1189 5110                     4
# # # … with 10,590 more rows
# traffic_sequence2 %>%
#   summarise(unique_per_sequence = length(unique(pedal_cycles_sequence)))




# Repeat counts -----------------------------------------------------------


### Points counted every single year 2010-2019
repeat_points = traffic_bam %>%
  filter(year %in% 2010:2019) %>%
  group_by(count_point_id) %>%
  tally() %>%
  filter(n >= 10)
# 3049
traffic_repeats = traffic_bam %>%
  filter(count_point_id %in% repeat_points$count_point_id) %>%
  filter(year %in% 2010:2019)
dim(traffic_repeats) #30490

### Points counted in 5 years 2010-2019
repeat_points = traffic_bam %>%
  filter(year %in% 2010:2019) %>%
  group_by(count_point_id) %>%
  tally() %>%
  filter(n >= 5) #5895
traffic_y5 = traffic_bam %>%
  filter(count_point_id %in% repeat_points$count_point_id) %>%
  filter(year %in% 2010:2019)
dim(traffic_y5) #51928

###filter out any count points that don't appear within both the first and second periods of 2009-2013 and 2014-2019?
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

traffic_el = traffic_bam %>%
  filter(count_point_id %in% repeat_points) %>%
  filter(year %in% 2010:2019)
dim(traffic_bam) #69971
dim(traffic_el) #68284

### 2010-2019 counts featuring all points that were surveyed in 2011 and at least one other year
points_2011 = traffic_bam %>%
  filter(year == 2011,
         # pedal_cycles > 0
         ) %>%
  group_by(count_point_id)
# 7884
traffic_with_2011 = traffic_bam %>%
  filter(count_point_id %in% points_2011$count_point_id)
dim(traffic_with_2011) #54444

# Get relative change in cycle counts, relative to 2011  -----------------------
t2011 = traffic_with_2011 %>%
  filter(year == 2011) %>%
  select(count_point_id, cycles_2011 = pedal_cycles)
t2011$cycles_2011[t2011$cycles_2011 == 0] = 1 #changing 0 counts to 1 to prevent problems of infite increase, while still including these count locations
traffic_with_2011 = traffic_with_2011 %>%
  inner_join(t2011) %>%
  mutate(change_from_2011 = pedal_cycles/cycles_2011)
# traffic_with_2011$change_from_2011[is.na(traffic_with_2011$change_from_2011)] = 0
sum(is.na(traffic_bam$change_from_2011))/nrow(traffic_bam) #0


# to_dim = traffic_bam %>%
#   filter(year %in% 2010:2019) %>%
#   group_by(count_point_id) %>%
#   tally() #30755


# Exploratory analysis ----------------------------------------------------

#Exploratory analysis
traffic_bam %>%
  group_by(road_category) %>%
  tally() %>%
  View()

traffic_repeats %>%
  group_by(road_category) %>%
  tally() %>%
  View()

forplot = traffic_bam %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
plot(pedal_cycles ~ year, data = forplot)

#### for graph
fortab = traffic_repeats %>%
  # filter(road_category == "TA") %>%
  group_by(year) %>%
  summarise(counts_10_yrs = mean(pedal_cycles),
            change_10_yrs = weighted.mean(change_cycles, w = mean_cycles))
# plot(pedal_cycles ~ year, data = fortab)

tab2 = traffic_bam %>%
  group_by(year) %>%
  summarise(counts_all = mean(pedal_cycles),
            change_all = weighted.mean(change_cycles, w = mean_cycles))

tab3 = traffic_y5 %>%
  group_by(year) %>%
  summarise(counts_5_yrs = mean(pedal_cycles),
            change_5_yrs = weighted.mean(change_cycles, w = mean_cycles))

fullgraph = inner_join(tab2, tab3) %>%
  inner_join(fortab)

ggplot(fullgraph, aes(x = year)) +
  geom_line(aes(y = counts_all, col = "darkred")) +
  geom_line(aes(y = counts_5_yrs, col = "green")) +
  geom_line(aes(y = counts_10_yrs, col = "steelblue")) +
  scale_color_discrete(name = "Years of data", labels = c("At least 2", "At least 5", "10")) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018)) +
  theme_grey() +
  labs(x = "Year", y = "Mean pedal cycle AADF")
  # guides(color=guide_legend("Years of data"))

ggplot(fullgraph, aes(x = year)) +
  geom_line(aes(y = change_all, col = "darkred")) +
  geom_line(aes(y = change_5_yrs, col = "green")) +
  geom_line(aes(y = change_10_yrs, col = "steelblue")) +
  scale_color_discrete(name = "Years of data", labels = c("At least 2", "At least 5", "10")) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018)) +
  theme_grey() +
  labs(x = "Year", y = "Mean change in pedal cycle AADF")

# change from 2011 graph
# change from a 2011 baseline has a very similar shape to change from the mean. So the results are not caused by 'shifting baseline syndrome'
# traffic_with_2011 = traffic_with_2011 %>%
#   filter(cycles_2011 > 5)

tab4 = traffic_with_2011 %>%
  group_by(year) %>%
  summarise(counts_all = mean(pedal_cycles),
            change_t2011 = weighted.mean(change_from_2011, w = cycles_2011),
            change_mean = weighted.mean(change_cycles, w = mean_cycles))

ggplot(tab4, aes(x = year)) +
  geom_line(aes(y = change_t2011, col = "darkred")) +
  geom_line(aes(y = change_mean, col = "steelblue")) +
  scale_color_discrete(name = "Change", labels = c("From 2011", "From mean")) +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018)) +
  theme_grey() +
  labs(x = "Year", y = "Mean change in pedal cycle AADF")
######

traffic_repeats %>%
  group_by(road_category) %>%
  tally()

#plot by year and road category
ttt = traffic_repeats %>% filter(road_category == "TA")
dim(ttt)
dim(ttt[ttt$pedal_cycles == 0,])

## all count points 2010-2019
all = traffic_2010_on %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
mb = traffic_2010_on %>%
  filter(road_category == "MB") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
pa = traffic_2010_on %>%
  filter(road_category == "PA") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
mcu = traffic_2010_on %>%
  filter(road_category == "MCU") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
ta = traffic_2010_on %>%
  filter(road_category == "TA") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))

plot(pedal_cycles ~ year, data = pa, col = "red", ylim = c(0, 350), type = "n")
lines(pedal_cycles ~ year, data = all, col = "black")
lines(pedal_cycles ~ year, data = pa, col = "red")
lines(pedal_cycles ~ year, data = mb, col = "blue")
lines(pedal_cycles ~ year, data = mcu, col = "green")
lines(pedal_cycles ~ year, data = ta, col = "yellow")
legend(2000, 350, legend = c("PA", "MB", "MCU", "TA"), col = c("red", "blue", "green", "yellow"), lty = 1)
title(main = "Mean cycle count across all count points 2010-2019", cex.main = 0.9)

##counts repeated every year 2010-2019
all = traffic_repeats %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
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
lines(pedal_cycles ~ year, data = all, col = "black")
lines(pedal_cycles ~ year, data = pa, col = "red")
lines(pedal_cycles ~ year, data = mb, col = "blue")
lines(pedal_cycles ~ year, data = mcu, col = "green")
lines(pedal_cycles ~ year, data = ta, col = "yellow")
legend(2017, 500, legend = c("All", "PA", "MB", "MCU", "TA"), col = c("black", "red", "blue", "green", "yellow"), lty = 1)
title(main = "Mean cycle count by road category for points sampled each year 2010 - 2019", cex.main = 0.8)

##counts in 2011 plus another year 2010-2019
all = traffic_with_2011 %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
mb = traffic_with_2011 %>%
  filter(road_category == "MB") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
pa = traffic_with_2011 %>%
  filter(road_category == "PA") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
mcu = traffic_with_2011 %>%
  filter(road_category == "MCU") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
ta = traffic_with_2011 %>%
  filter(road_category == "TA") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))

plot(pedal_cycles ~ year, data = pa, col = "red", ylim = c(0, 500), type = "n")
lines(pedal_cycles ~ year, data = all, col = "black")
lines(pedal_cycles ~ year, data = pa, col = "red")
lines(pedal_cycles ~ year, data = mb, col = "blue")
lines(pedal_cycles ~ year, data = mcu, col = "green")
lines(pedal_cycles ~ year, data = ta, col = "yellow")
legend(2017, 500, legend = c("All", "PA", "MB", "MCU", "TA"), col = c("black", "red", "blue", "green", "yellow"), lty = 1)
title(main = "Mean cycle count by road category for points sampled in 2011 and another year 2010-2019", cex.main = 0.8)

#####London

traffic_london = traffic_repeats %>%
  filter(name %in% lads$Name)

##counts repeated every year 2010-2019
all = traffic_london %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
mb = traffic_london %>%
  filter(road_category == "MB") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
pa = traffic_london %>%
  filter(road_category == "PA") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
mcu = traffic_london %>%
  filter(road_category == "MCU") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
ta = traffic_london %>%
  filter(road_category == "TA") %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))

plot(pedal_cycles ~ year, data = pa, col = "red", ylim = c(0, 1000), type = "n")
lines(pedal_cycles ~ year, data = all, col = "black")
lines(pedal_cycles ~ year, data = pa, col = "red")
lines(pedal_cycles ~ year, data = mb, col = "blue")
lines(pedal_cycles ~ year, data = mcu, col = "green")
lines(pedal_cycles ~ year, data = ta, col = "yellow")
legend(2010, 1000, legend = c("All", "PA", "MB", "MCU", "TA"), col = c("black", "red", "blue", "green", "yellow"), lty = 1)
title(main = "Mean cycle count by road category for points sampled each year 2010 - 2019", cex.main = 0.8)

# road categories by year
cats = traffic_bam %>%
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
  xlab("Year") + ylab("Number of counts") +
scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018))




# GAM models --------------------------------------------------------------


M = list(c(1, 0.5), NA)




# randomise the row order of the data
set.seed(42)
new_order = sample(nrow(traffic_bam))
traffic_bam = traffic_bam[new_order, ]

sample_bam = traffic_bam[1:50000,]

system.time({m1 = bam(pedal_cycles ~
                        s(year, bs = "cr", k = 5) +
                        s(road_category, bs = "re") +
                        s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5)),
                        # ti(easting, northing, year, d = c(2,1), bs = c('ds','cr'), m = M, k = c(25, 3)), #not significant
                        # ti(year, road_category, bs = c("tp", "re")),
                      family = nb(link = "log"),
                      data = traffic_bam, method = 'fREML',
                      nthreads = 4, discrete = TRUE)}) #12 seconds
summary(m1)
plot(m1, pages = 3, scheme = 2, shade = TRUE)

system.time({m4 = bam(pedal_cycles ~
                        s(year, bs = "cr", k = 5) +
                        s(road_category, bs = "re") +
                        s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5)) +
                        ti(easting, northing, year, d = c(2,1), bs = c('ds','cr'), m = M, k = c(100, 3)), #not significant
                      # ti(year, road_category, bs = c("tp", "re")),
                      family = nb(link = "log"),
                      data = traffic_2010_on, method = 'fREML',
                      nthreads = 4, discrete = TRUE)}) #12 seconds
summary(m4)
plot(m4, pages = 1, scheme = 2, shade = TRUE)

AIC(m1, m4)

system.time({m2 = bam(pedal_cycles ~
                        s(year, bs = "cr", k = 5) +
                        s(road_category, bs = "re"),
                        # s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5)),
                        # ti(easting, northing, year, d = c(2,1), bs = c('ds','cr'), m = M, k = c(25, 3)), #not significant
                      # ti(year, road_category, bs = c("tp", "re")),
                      family = nb(link = "log"),
                      data = traffic_repeats, method = 'fREML',
                      nthreads = 4, discrete = TRUE)}) #12 seconds
summary(m2)
plot(m2, pages = 1, scheme = 2, shade = TRUE)

# library(nlme)
# system.time({m4 = lme(pedal_cycles ~ year,
#                       family = nb(link = "log"),
#                       data = sample_bam,
#                       random = road_category)}) #102 seconds
# summary(m4)
# plot(m4, pages = 1, scheme = 2, shade = TRUE)
#
# library(glmmTMB)
# system.time({m2 <- glmmTMB(pedal_cycles~year+(1|road_category),
#                          data=sample_bam,
#                          family=nbinom1)}) #
# plot(m2, pages = 1, scheme = 2, shade = TRUE)


# GAM model for London 2010-2019 ------------------------------------------



# Filter London counts
# Assign count points to 1km grid squares
traffic_london_bam = traffic_2010_on %>%
  filter(name %in% lads$Name)
dim(traffic_london_bam) #8130

ml = bam(pedal_cycles ~
          s(year, bs = "cr", k = 5) +
          s(road_category, bs = 're') +
          s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5)) +
          ti(easting, northing, year, d = c(2,1), bs = c('ds','tp'),
             m = M, k = c(25, 5)),
        family = nb(link = "log"),
        data = traffic_london_bam, method = 'fREML',
        nthreads = 4, discrete = TRUE)
summary(ml)
plot(ml, pages = 4, scheme = 2, shade = TRUE)

# old unnused code:

# # just about works - but simpler approach used
# traffic_bam %>%
#   mutate(name = as.character(name)) %>%
#   group_by(year, name) %>%
#   summarise(change_cycles = weighted.mean(change_cycles, w = mean_cycles),
#             mean_cycles = mean(mean_cycles)) %>%
#   mutate(name = case_when(
#     name %in% las_of_interest ~ name,
#     TRUE ~ "Other")
#   ) %>%
#   ggplot(., aes(x = year, y = change_cycles, group = name, colour = name)) +
#   geom_line(aes(alpha = mean_cycles/100)) +
#   ylim(c(0,2))
