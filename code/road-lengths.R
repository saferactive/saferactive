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


