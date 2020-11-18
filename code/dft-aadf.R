# Takes input from `geographic-data-cleaning.R`

##Latest DfT dataset including 2019 counts
library(tidyverse)
library(mgcv)
library(viridis)

piggyback::pb_download("traffic_cyclable_clean.Rds")
traffic_cyclable = readRDS("traffic_cyclable_clean.Rds")
dim(traffic_cyclable)
# [1] 183884     35 439688

traffic_points = traffic_cyclable %>%
  select(year, name, count_point_id, road_category, easting, northing, pedal_cycles, estimation_method, estimation_method_detailed, link_length_km) %>%
  group_by(year, name, count_point_id, road_category, easting, northing, estimation_method, estimation_method_detailed, link_length_km) %>%
  summarise(pedal_cycles = sum(pedal_cycles)) %>%
  ungroup()
# nrow(traffic_points) / nrow(traffic_cyclable)
# # [1] 0.3980621 # why are there multiple reading for each count point?
# skimr::skim(traffic_cyclable)

# check points are measured every year
# traffic_points %>%
#   # filter(estimation_method == "Counted") %>%
#   group_by(year) %>%
#   tally()

#do this unless we are taking average counts for LAs
traffic_points = traffic_points %>%
  filter(estimation_method_detailed != "Dependent on a neighbouring counted link")

traffic_bam = transform(traffic_points,
                        count_point_id = factor(count_point_id),
                        name = factor(name),
                        road_category = factor(road_category),
                        link_length_km = as.numeric(link_length_km))

# Assign count points to 500m grid squares # this has to use `round()` instead of `signif()`
traffic_bam = traffic_bam %>%
  mutate(grid_location = paste((round((traffic_bam$easting*2), digits = -3)/2),(round((traffic_bam$northing*2), digits = -3)/2)))

# dim(traffic_bam) #177313
# length(unique(traffic_bam$grid_location))

# Remove pre-2010 counts and count points with only 1 year or less of counted data --------

repeat_points = traffic_bam %>%
  filter(year %in% 2010:2019,
         estimation_method == "Counted") %>%
  group_by(count_point_id) %>%
  tally() %>%
  filter(n > 1)
#12668
traffic_bam = traffic_bam %>%
  filter(count_point_id %in% repeat_points$count_point_id) %>%
  filter(year %in% 2010:2019)
dim(traffic_bam) #67614 #124767

##surveyed in 2011
repeat_points = traffic_bam %>%
  filter(year == 2011) %>%
  group_by(count_point_id) %>%
  tally()
traffic_with_2011 = traffic_bam %>%
  filter(count_point_id %in% repeat_points$count_point_id) %>%
  filter(year %in% 2010:2019)
dim(traffic_with_2011) #53275 #123921


# Get relative change in cycle counts  ------------------------------------

traffic_bam = traffic_bam %>%
  group_by(count_point_id) %>%
  mutate(mean_cycles = mean(pedal_cycles),
         mean_year = mean(year),
         n_year = n()) %>%
  ungroup() %>%
  filter(mean_cycles > 0) %>% # remove counts where there has never been a cyclist
  mutate(change_cycles = pedal_cycles/mean_cycles)
# traffic_bam$change_cycles[is.na(traffic_bam$change_cycles)] = 0 #this creates erroneous data
sum(is.na(traffic_bam$change_cycles))/nrow(traffic_bam) #0
dim(traffic_bam) #66323 #122456
# nas = traffic_bam %>%
#   filter(is.na(change_cycles))
# View(nas)

# remove points with at least one year of 0 count
# remove points with mean_cycles <5
traffic_0 = traffic_bam %>%
  group_by(count_point_id) %>%
  filter(pedal_cycles == 0)
traffic_nonzero = traffic_bam %>%
  filter(! count_point_id %in% traffic_0$count_point_id,
         mean_cycles >= 5.0)
dim(traffic_nonzero) #51753 #99130



traffic_nonzero = traffic_nonzero %>%
  group_by(grid_location, year) %>%
  mutate(smooth_change = mean(change_cycles),
         smooth_mean = mean(mean_cycles)) %>%
  ungroup()

traffic_bam %>%
  group_by(year) %>%
  summarise(change_cycles = weighted.mean(change_cycles, w = mean_cycles)) %>%
  ggplot(., aes(x = year, y = change_cycles)) +
  geom_line()

traffic_bam %>%
  group_by(name) %>%
  mutate(sum_cycles = sum(pedal_cycles)) %>% # sum across all years
  ungroup() %>%
  group_by(year, name, sum_cycles) %>%
  summarise(change_cycles = weighted.mean(change_cycles, w = mean_cycles),
            mean_cycles = mean(mean_cycles)) %>%
  ggplot(., aes(x = year, y = change_cycles, group = name)) +
  geom_line(aes(alpha = sum_cycles)) +
  ylim(c(0,2))

las_of_interest = c("Leeds", "Derby", "Southampton",
                    "Nottingham", "Birmingham")



# Get London Borough boundaries -------------------------------------------

lads = readRDS("lads.Rds")
boroughs = as.character(spData::lnd$NAME)
lads = lads %>%
  filter(Name %in% boroughs)

traffic_london = traffic_bam %>%
  inner_join(lads, by = c("name" = "Name"))
dim(traffic_london) #5215 6390 with zeroes #13333 with estimated

dft_counts_by_borough = traffic_london %>%
       group_by(name, year) %>%
       summarise(pedal_cycles = mean(pedal_cycles),
                 change_cycles = mean(change_cycles),
                 n_year = mean(n_year),
                 n_counts = n())
# View(dft_counts_by_borough)

saveRDS(dft_counts_by_borough, "dft-counts-by-borough-with-esti.Rds")

dft_counts_by_borough_no_esti = traffic_london %>%
  filter(estimation_method == "Counted") %>%
  group_by(name, year) %>%
  summarise(pedal_cycles = mean(pedal_cycles),
            change_cycles = mean(change_cycles),
            n_year = mean(n_year),
            n_counts = n())

saveRDS(dft_counts_by_borough, "dft-counts-by-borough-no-esti.Rds")


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


# Multiplier of change from 2011 ------------------------------------------


### 2010-2019 counts featuring all points that were surveyed in 2010-2012 and at least one other year
points_2011 = traffic_london %>%
  filter(year %in% 2010:2012,
         # pedal_cycles > 0
  ) %>%
  group_by(count_point_id)
dim(points_2011) # 1671
repeat_points = traffic_london %>%
  filter(count_point_id %in% points_2011$count_point_id) %>%
  group_by(count_point_id) %>%
  tally() %>%
  filter(n >= 2) #986
traffic_with_2011 = traffic_london %>%
  filter(count_point_id %in% repeat_points$count_point_id)
dim(traffic_with_2011) #54444 #4705 in london

# Get change in cycle counts, relative to 2010-12  -----------------------
t2011 = unique(traffic_with_2011 %>%
  filter(year %in% 2010:2012) %>%
  group_by(count_point_id) %>%
  mutate(cycles_2011 = mean(pedal_cycles)) %>%
  ungroup() %>%
  select(count_point_id, cycles_2011))

# t2011$cycles_2011[t2011$cycles_2011 == 0] = 1 #changing 0 counts to 1 to prevent problems of infinite increase, while still including these count locations. no longer required.
traffic_with_2011 = traffic_with_2011 %>%
  inner_join(t2011) %>%
  mutate(change_from_2011 = pedal_cycles/cycles_2011)
# traffic_with_2011$change_from_2011[is.na(traffic_with_2011$change_from_2011)] = 0
sum(is.na(traffic_with_2011$change_from_2011))/nrow(traffic_with_2011) #0

# # calculate change in cycling uptake relative to 2011
# traffic_with_2011 = traffic_london %>%
#   mutate(cycling_2011 = case_when(year %in% 2010:2012 ~ change_cycles)) %>%
#   group_by(count_point_id) %>%
#   mutate(multiplier = change_cycles / mean(cycling_2011, na.rm = TRUE)) %>%
#   ungroup()

# to_dim = traffic_bam %>%
#   filter(year %in% 2010:2019) %>%
#   group_by(count_point_id) %>%
#   tally() #30755

hist(traffic_with_2011$pedal_cycles, breaks = 500)
hist(traffic_with_2011$change_cycles, breaks = 500)
hist(traffic_with_2011$change_from_2011, breaks = 500)
hist(traffic_with_2011$logchange, breaks = 500)

sd(traffic_with_2011$pedal_cycles)
sd(traffic_with_2011$change_cycles)
sd(traffic_with_2011$change_from_2011)
boxplot(traffic_with_2011$pedal_cycles)
boxplot(traffic_with_2011$change_cycles)
boxplot(traffic_with_2011$change_from_2011)
boxplot(traffic_with_2011$logchange)

traffic_with_2011$logchange = log(traffic_with_2011$change_cycles)

# Repeat counts -----------------------------------------------------------


### Points counted every single year 2010-2019
repeat_points = traffic_london %>%
  filter(year %in% 2010:2019) %>%
  group_by(count_point_id) %>%
  tally() %>%
  filter(n >= 10)
# 3049 132 london
traffic_repeats = traffic_london %>%
  filter(count_point_id %in% repeat_points$count_point_id) %>%
  filter(year %in% 2010:2019)
dim(traffic_repeats) #30490 1320 london

### Points counted in 5 years 2010-2019
repeat_points = traffic_london %>%
  filter(year %in% 2010:2019) %>%
  group_by(count_point_id) %>%
  tally() %>%
  filter(n >= 5) #5895 394 london
traffic_y5 = traffic_london %>%
  filter(count_point_id %in% repeat_points$count_point_id) %>%
  filter(year %in% 2010:2019)
dim(traffic_y5) #51928 3277 london

###filter out any count points that don't appear within both the first and second periods of 2009-2013 and 2014-2019?
early_year = traffic_nonzero %>%
  group_by(count_point_id, year) %>%
  filter(year %in% 2010:2014)
early = unique(early_year$count_point_id)
length(early)

late_year = traffic_nonzero %>%
  group_by(count_point_id, year) %>%
  filter(year %in% 2015:2019)
late = unique(late_year$count_point_id)
length(late)

repeat_points = intersect(early, late)

traffic_el = traffic_nonzero %>%
  filter(count_point_id %in% repeat_points) %>%
  filter(year %in% 2010:2019)
dim(traffic_nonzero) #51753
dim(traffic_el) #50799




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

tab2 = traffic_london %>%
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

## weighted mean change - counts in at least 2 years 2010-2019
# change this to ggplot
all = traffic_bam %>%
  group_by(year) %>%
  summarise(pedal_cycles = weighted.mean(change_cycles, w = mean_cycles))
mb = traffic_bam %>%
  filter(road_category == "MB") %>%
  group_by(year) %>%
  summarise(pedal_cycles = weighted.mean(change_cycles, w = mean_cycles))
pa = traffic_bam %>%
  filter(road_category == "PA") %>%
  group_by(year) %>%
  summarise(pedal_cycles = weighted.mean(change_cycles, w = mean_cycles))
mcu = traffic_bam %>%
  filter(road_category == "MCU") %>%
  group_by(year) %>%
  summarise(pedal_cycles = weighted.mean(change_cycles, w = mean_cycles))
ta = traffic_bam %>%
  filter(road_category == "TA") %>%
  group_by(year) %>%
  summarise(pedal_cycles = weighted.mean(change_cycles, w = mean_cycles))

plot(pedal_cycles ~ year, data = pa, col = "red", ylim = c(0.7, 1.2), type = "n")
lines(pedal_cycles ~ year, data = all, col = "black")
lines(pedal_cycles ~ year, data = pa, col = "red")
lines(pedal_cycles ~ year, data = mb, col = "blue")
lines(pedal_cycles ~ year, data = mcu, col = "green")
lines(pedal_cycles ~ year, data = ta, col = "yellow")
legend(2017, 0.9, legend = c("PA", "MB", "MCU", "TA"), col = c("red", "blue", "green", "yellow"), lty = 1)
title(main = "Change in AADF across all count points 2010-2019", cex.main = 0.9)

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
all = traffic_london %>%
  group_by(year) %>%
  summarise(pedal_cycles = weighted.mean(change_cycles, w = mean_cycles))
mb = traffic_london %>%
  filter(road_category == "MB") %>%
  group_by(year) %>%
  summarise(pedal_cycles = weighted.mean(change_cycles, w = mean_cycles))
pa = traffic_london %>%
  filter(road_category == "PA") %>%
  group_by(year) %>%
  summarise(pedal_cycles = weighted.mean(change_cycles, w = mean_cycles))
mcu = traffic_london %>%
  filter(road_category == "MCU") %>%
  group_by(year) %>%
  summarise(pedal_cycles = weighted.mean(change_cycles, w = mean_cycles))
ta = traffic_london %>%
  filter(road_category == "TA") %>%
  group_by(year) %>%
  summarise(pedal_cycles = weighted.mean(change_cycles, w = mean_cycles))

plot(pedal_cycles ~ year, data = pa, col = "red", ylim = c(0.8, 1.3), type = "n")
lines(pedal_cycles ~ year, data = all, col = "black")
lines(pedal_cycles ~ year, data = pa, col = "red")
lines(pedal_cycles ~ year, data = mb, col = "blue")
lines(pedal_cycles ~ year, data = mcu, col = "green")
lines(pedal_cycles ~ year, data = ta, col = "yellow")
legend(2017, 500, legend = c("All", "PA", "MB", "MCU", "TA"), col = c("black", "red", "blue", "green", "yellow"), lty = 1)
title(main = "Mean cycle count by road category for points sampled in 2011 and another year 2010-2019", cex.main = 0.8)

##weighted mean flow - counts in at least two years 2010-2019. weighted by mean cycles
all = traffic_y5 %>%
  group_by(year) %>%
  summarise(pedal_cycles = weighted.mean(change_cycles, w = mean_cycles))
mb = traffic_y5 %>%
  filter(road_category == "MB") %>%
  group_by(year) %>%
  summarise(pedal_cycles = weighted.mean(change_cycles, w = mean_cycles))
pa = traffic_y5 %>%
  filter(road_category == "PA") %>%
  group_by(year) %>%
  summarise(pedal_cycles = weighted.mean(change_cycles, w = mean_cycles))
mcu = traffic_y5 %>%
  filter(road_category == "MCU") %>%
  group_by(year) %>%
  summarise(pedal_cycles = weighted.mean(change_cycles, w = mean_cycles))
ta = traffic_y5 %>%
  filter(road_category == "TA") %>%
  group_by(year) %>%
  summarise(pedal_cycles = weighted.mean(change_cycles, w = mean_cycles))

plot(pedal_cycles ~ year, data = pa, col = "red", ylim = c(0.8, 1.3), type = "n")
lines(pedal_cycles ~ year, data = all, col = "black")
lines(pedal_cycles ~ year, data = pa, col = "red")
lines(pedal_cycles ~ year, data = mb, col = "blue")
lines(pedal_cycles ~ year, data = mcu, col = "green")
lines(pedal_cycles ~ year, data = ta, col = "yellow")
legend(2010, 350, legend = c("All", "PA", "MB", "MCU", "TA"), col = c("black", "red", "blue", "green", "yellow"), lty = 1)
title(main = "Mean cycle count by road category for points sampled in 2011 and another year 2010-2019", cex.main = 0.8)


#####London

# traffic_london = traffic_repeats %>%
#   filter(name %in% lads$Name)

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

plot(pedal_cycles ~ year, data = pa, col = "red", ylim = c(0, 2000), type = "n")
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

system.time({m = bam(change_from_2011 ~
                        s(year, bs = "cr", k = 5)
                        + s(road_category, bs = "re")
                        # + s(mean_year, bs = "cr", k = 3)
                        + s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5))
                        + ti(easting, northing, year, d = c(2,1), bs = c('ds','cr'), m = M, k = c(25, 3))
                      ,
                      # ti(year, road_category, bs = c("tp", "re")),
                      weights = mean_cycles,
                      family = scat,
                      data = traffic_with_2011, method = 'fREML',
                      nthreads = 4, discrete = TRUE)}) #15 seconds
summary(m)
plot(m, pages = 4, scheme = 2, shade = TRUE)

View(traffic_with_2011 %>%
  group_by(name, year) %>%
  summarise(change_from_2011 = mean(change_from_2011)))

gam.check(m)
plot(fitted(m), residuals(m))
plot(traffic_nonzero$year, residuals(m))
plot(traffic_nonzero$easting, residuals(m))
plot(traffic_nonzero$northing, residuals(m))
plot(traffic_nonzero$mean_year, residuals(m))
qqnorm(residuals(m))
qqline(residuals(m))

traffic_nonzero$boot = boot::inv.logit(traffic_nonzero$change_cycles)
traffic_nonzero$logo = log(traffic_nonzero$change_cycles)

# assign the framework that will be used as a basis for predictions
pdata = with(traffic_with_2011,
             expand.grid(year = seq(min(year), max(year), by = 1),
                         road_category = "PA",
                         easting = seq(round((min(easting)*2), digits = -3)/2, round((max(easting)*2), digits = -3)/2, by = 500), # changed this to make regular 1km grid squares
                         northing = seq(round((min(northing)*2), digits = -3)/2, round((max(northing)*2), digits = -3)/2, by = 500)))
# make predictions according to the GAM model
fitted = predict(m, newdata = pdata, type = "response", exclude = "road_category", newdata.guaranteed = TRUE)
# predictions for points far from any counts set to NA
ind = exclude.too.far(pdata$easting, pdata$northing,
                      traffic_london$easting, traffic_london$northing, dist = 0.02)
fitted[ind] = NA
# join the predictions with the framework data
pred_all_points_year = cbind(pdata, Fitted = fitted)
pred_all_points_year = pred_all_points_year %>%
  drop_na

#################

system.time({m2 = bam(change_cycles ~
                        s(year, bs = "cr", k = 5)
                      + s(mean_year, bs = "cr", k = 3)
                      + s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5))
                      + ti(easting, northing, year, d = c(2,1), bs = c('ds','cr'), m = M, k = c(25, 3))
                      ,
                      weights = (mean_cycles*n_year),
                      # weights = (mean_cycles),
                      family = scat,
                      data = traffic_london, method = 'fREML',
                      nthreads = 4, discrete = TRUE)}) #15 seconds
summary(m2)
plot(m2, pages = 4, scheme = 2, shade = TRUE)

gam.check(m2)
plot(fitted(m2), residuals(m2))
plot(traffic_london$year, residuals(m2))
plot(traffic_london$easting, residuals(m2))
plot(traffic_london$northing, residuals(m2))
plot(traffic_london$mean_year, residuals(m2))
qqnorm(residuals(m2))
qqline(residuals(m2))


# assign the framework that will be used as a basis for predictions
pdata = with(traffic_london,
             expand.grid(year = seq(min(year), max(year), by = 1),
                         mean_year = 2012,
                         easting = seq(round((min(easting)*2), digits = -3)/2, round((max(easting)*2), digits = -3)/2, by = 500), # changed this to make regular 1km grid squares
                         northing = seq(round((min(northing)*2), digits = -3)/2, round((max(northing)*2), digits = -3)/2, by = 500)))
# make predictions according to the GAM model
fitted2 = predict(m2, newdata = pdata, type = "response",
                  exclude = "mean_year",
                  newdata.guaranteed = TRUE)
# predictions for points far from any counts set to NA
ind = exclude.too.far(pdata$easting, pdata$northing,
                      traffic_london$easting, traffic_london$northing, dist = 0.02)
fitted2[ind] = NA
# join the predictions with the framework data
pred_all_points_year = cbind(pdata, Fitted = fitted2)
pred_all_points_year = pred_all_points_year %>%
  drop_na



ggplot(pred_all_points_year, aes(x = easting, y = northing)) +
  geom_raster(aes(fill = Fitted)) + facet_wrap(~ year, ncol = 5) +
  scale_fill_viridis(name = "Change in pedal cycles", option = 'plasma',
                     na.value = 'transparent') +
  # geom_rect(xmin = 504000, xmax = 557500, ymin = 157500, ymax = 200000) + #doesn't work, maybe geom_tile would? but geom_raster is probably fine
  coord_fixed(ratio = 1) +
  # geom_line(lads, alpha(0.1)) +
  theme(legend.position = 'top', legend.key.width = unit(2, 'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

saveRDS(pred_all_points_year, "pred-london-change-cycles.Rds")

borough_geom = lads %>%
  dplyr::select(Name) %>%
  st_transform(27700)

## Assign to borough for predictions by year
pred_sf = pred_all_points_year %>%
  st_as_sf(coords = c("easting", "northing"), crs = 27700)
point_to_borough = st_join(x = pred_sf, y = borough_geom)
## Calculate mean annual predictions for each borough
pb_preds_year = point_to_borough %>%
  drop_na() %>%
  group_by(Name, year) %>%
  summarise(borough_change_cycles = mean(Fitted))
View(pb_preds_year)

#change per borough relative to 2015 (for TfL verification)
pb_preds_year = pb_preds_year %>%
  mutate(baseline_2015 = case_when(
    year == 2015 ~ borough_change_cycles)) %>%
  group_by(Name) %>%
  mutate(pred_relative_to_2015 = borough_change_cycles/mean(baseline_2015, na.rm = TRUE)) %>%
  ungroup()

saveRDS(pb_preds_year, "pred-borough-change-cycles.Rds")


#########


system.time({m3 = bam(pedal_cycles ~
                        s(year, bs = "cr", k = 5)
                      # + s(road_category, bs = "re")
                      + s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5))
                      + ti(easting, northing, year, d = c(2,1), bs = c('ds','cr'), m = M, k = c(25, 3))
                      ,
                      # weights = (n_year),
                      family = nb(link = "log"),
                      data = traffic_london, method = 'fREML',
                      nthreads = 4, discrete = TRUE)}) #15 seconds
summary(m3)
plot(m3, pages = 4, scheme = 2, shade = TRUE)

gam.check(m3)
plot(fitted(m2), residuals(m2))
plot(traffic_london$year, residuals(m2))
plot(traffic_london$easting, residuals(m2))
plot(traffic_london$northing, residuals(m2))
plot(traffic_london$mean_year, residuals(m2))
qqnorm(residuals(m2))
qqline(residuals(m2))


# assign the framework that will be used as a basis for predictions
pdata = with(traffic_london,
             expand.grid(year = seq(min(year), max(year), by = 1),
                         easting = seq(round((min(easting)*2), digits = -3)/2, round((max(easting)*2), digits = -3)/2, by = 500), # changed this to make regular 1km grid squares
                         northing = seq(round((min(northing)*2), digits = -3)/2, round((max(northing)*2), digits = -3)/2, by = 500)))
# make predictions according to the GAM model
fitted = predict(m3, newdata = pdata, type = "response", exclude = "road_category", newdata.guaranteed = TRUE)
# predictions for points far from any counts set to NA
ind = exclude.too.far(pdata$easting, pdata$northing,
                      traffic_london$easting, traffic_london$northing, dist = 0.02)
fitted[ind] = NA
# join the predictions with the framework data
pred_all_points_year = cbind(pdata, Fitted = fitted)
pred_all_points_year = pred_all_points_year %>%
  drop_na


ggplot(pred_all_points_year, aes(x = easting, y = northing)) +
  geom_raster(aes(fill = Fitted)) + facet_wrap(~ year, ncol = 5) +
  viridis::scale_fill_viridis(name = "Pedal cycles", option = 'plasma',
                     na.value = 'transparent') +
  coord_fixed(ratio = 1) +
  # geom_line(lads, alpha(0.1)) +
  theme(legend.position = 'top', legend.key.width = unit(2, 'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


borough_geom = lads %>%
  dplyr::select(Name) %>%
  st_transform(27700)

## Assign to borough for predictions by year
pred_sf = pred_all_points_year %>%
  st_as_sf(coords = c("easting", "northing"), crs = 27700)
point_to_borough = st_join(x = pred_sf, y = borough_geom)
## Calculate mean annual predictions for each borough
pb_preds_year = point_to_borough %>%
  drop_na() %>%
  group_by(Name, year) %>%
  summarise(borough_mean_cycles = mean(Fitted))
View(pb_preds_year)

saveRDS(pb_preds_year, "london-dft-gam-preds-with-esti.Rds")

## Compare GAM predictions with raw counts, by borough and year

verify_dft_gam = inner_join(pb_preds_year, dft_counts_by_borough, by = c("Name" = "name", "year"))

cor(verify_dft_gam$pedal_cycles, verify_dft_gam$borough_mean_cycles)^2 #R squared = 0.946
plot(verify_dft_gam$pedal_cycles, verify_dft_gam$borough_mean_cycles, xlab = "Mean DfT cycle count AADF (including estimated counts)", ylab = "GAM predictions based on DfT counts"
     # , xlim = c(0,2000), ylim = c(0,2000)
     )
text(verify_dft_gam$pedal_cycles, verify_dft_gam$borough_mean_cycles, verify_dft_gam$year, pos = 1, cex = 0.7)
text(verify_dft_gam$pedal_cycles, verify_dft_gam$borough_mean_cycles, verify_dft_gam$Name, pos = 1, cex = 0.7)

# compare with actual counts only, not estimated counts
verify_dft_gam = inner_join(pb_preds_year, dft_counts_by_borough_no_esti, by = c("Name" = "name", "year"))

cor(verify_dft_gam$pedal_cycles, verify_dft_gam$borough_mean_cycles)^2 #R squared = 0.872
plot(verify_dft_gam$pedal_cycles, verify_dft_gam$borough_mean_cycles, xlab = "Mean DfT cycle count AADF (including estimated counts)", ylab = "GAM predictions based on DfT counts")
text(verify_dft_gam$pedal_cycles, verify_dft_gam$borough_mean_cycles, verify_dft_gam$year, pos = 1, cex = 0.7)
text(verify_dft_gam$pedal_cycles, verify_dft_gam$borough_mean_cycles, verify_dft_gam$Name, pos = 1, cex = 0.7)

############

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
