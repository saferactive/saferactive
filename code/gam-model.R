# Processing of DfT raw count data
# Was originally for London only, but now includes whole country.
# Uses output from `geographical-data-cleaning.R`

# File also includes GAM model for DfT counts only. GAM including DfT and TfL counts is in `dft-tfl-together.R`

library(tidyverse)
library(mgcv)
library(ggplot2)
library(viridis)
library(mapview)
theme_set(theme_bw())
library(ggpubr)


# # Older dataset
# piggyback::pb_download("traffic_london.Rds")
# traffic_london = readRDS("traffic_london.Rds")

traffic_latest = readRDS("traffic_cyclable_clean_raw.Rds")

# traffic_u = traffic_latest %>%
#   unique()
# dim(traffic_u) #4321788

# Get local authority names
traffic_latest = traffic_latest %>%
  select(year, count_date, hour, name, count_point_id, easting, northing, pedal_cycles, road_category, direction_of_travel)

# remove motorways
traffic_latest = traffic_latest %>%
  filter(road_category != "TM",
         road_category != "PM")
dim(traffic_latest) #4321788

traffic_latest = traffic_latest %>%
  unique()
dim(traffic_latest) #4155984

# Fix points with location errors
# traffic_latest %>% filter(count_point_id == 946853) %>%
#   sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>% mapview()
# traffic_latest %>% filter(count_point_id == 952939) %>%
#   sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>% mapview()

error1 = traffic_latest %>%
  filter(count_point_id == 946853)
error2 = traffic_latest %>%
  filter(count_point_id == 952939)
error3 = traffic_latest %>%
  filter(count_point_id == 38565)
error4 = traffic_latest %>%
  filter(count_point_id == 942684)
error5 = traffic_latest %>%
  filter(count_point_id == 942350)
error6 = traffic_latest %>%
  filter(count_point_id == 81119)
error7 = traffic_latest %>%
  filter(count_point_id == 942920)

error1[error1$easting == 135809,] = error1[error1$easting == 135809,] %>%
  mutate(northing = 24870)
error2 = error2 %>%
  mutate(northing = 221460)
error3 = error3 %>%
  mutate(northing = 178027, easting = 530470) %>%
  unique()
error4 = error4 %>%
  mutate(northing = 177575, easting = 509142) %>%
  unique()
error5 = error5 %>%
  mutate(northing = 197938, easting = 533059) %>%
  unique()
error6 = error6 %>%
  mutate(northing = 180950, easting = 530640) %>%
  unique()
error7 = error7 %>%
  mutate(road_category = "MB") %>%
  unique()

traffic_corrected = traffic_latest %>%
  filter(count_point_id != 952939,
         count_point_id != 946853,
         count_point_id != 38565,
         count_point_id != 942684,
         count_point_id != 942350,
         count_point_id != 81119,
         count_point_id != 942920
         )
traffic_corrected = rbind(traffic_corrected, error1, error2, error3,
                          error4, error5,
                          error6, error7)
dim(traffic_corrected) #4315296 #4155876

# Filter to use London data only
boroughs = as.character(spData::lnd$NAME)

t_london = traffic_corrected %>%
  filter(name %in% boroughs)
dim(t_london) #370500 #354864

# using bidirectional flows to match PCT, but this doesn't really reduce the increased residuals (+ve and -ve) at high predicted values (ie extreme variance in central london cycle flows)

# # Remove counts with hour errors
# traffic_bam = traffic_bam %>%
#   filter(hour %in% 7:18)

# # use peak hours only
# traffic_peak_only = t_london %>%
#   filter(hour %in% c(7, 8, 9, 16, 17, 18))
# dim(traffic_peak_only) #185250 #177432

# # use peak hours only
# traffic_peak_only = traffic_corrected %>%
#   filter(hour %in% c(7, 8, 9, 16, 17, 18))
# dim(traffic_peak_only) #2077931

# check each count point is consistently either bi-directional or unidirectional
wrong = traffic_corrected %>%
  group_by(year, count_date, count_point_id) %>%
  summarise(
    n_periods = n()
  )
unique(wrong$n)
# [1] 12  6

#check
View(wrong %>%
       group_by(n_periods) %>%
       tally())

# remove those not consistently bidirectional or unidirectional
traffic_corrected = left_join(traffic_corrected, wrong, by = c("year", "count_date", "count_point_id")) %>%
  filter(n_periods == 12 | n_periods == 24)
dim(traffic_corrected) #4146852

# check2 = inner_join(traffic_corrected, ford, by = c("year", "count_point_id", "count_date"))
# ddd = check2 %>% group_by(count_point_id) %>%
#   length(distinct(check2$n))
# unique(ddd$n)

# # # check 4-way points
# wrong[wrong$n == 48,]
# ford = wrong %>% filter(n == 24)
# fd = traffic_corrected %>% filter(count_point_id == 940071)
# fd %>% group_by(year) %>% tally()
# View(fd %>% filter(year == 2018))
#
# traffic_corrected = traffic_corrected %>%
#   filter(! count_point_id %in% ford$count_point_id)
# t11 = wrong %>% filter(n == 11)
# traffic_corrected = traffic_corrected %>%
#   filter(! count_point_id %in% t11$count_point_id)
#
# dim(traffic_corrected) #2050446
# # fd = fd %>%
# #   select(-easting, -northing)

# combine bidirectional counts
traffic_london_points = traffic_corrected %>%
  group_by(year, count_date, hour, name, count_point_id, easting, northing) %>%
  summarise(pedal_cycles = sum(pedal_cycles)) %>% #sums movements in both directions
  ungroup()

traffic_london_bam = transform(traffic_london_points,
                       count_point_id = factor(count_point_id),
                       name = factor(name),
                       DoY = as.numeric(lubridate::yday(count_date)))

# seasonally adjust pedal cycles
q1 = c("01", "02", "03")
q2 = c("04", "05", "06")
q3 = c("07", "08", "09")
q4 = c("10", "11", "12")

traffic_london_bam = traffic_london_bam %>%
  mutate(pedal_cycles_adj = case_when(
    substr(count_date, 6, 7) %in% q1 ~ pedal_cycles*1.14,
    substr(count_date, 6, 7) %in% q2 ~ pedal_cycles*0.93,
    substr(count_date, 6, 7) %in% q3 ~ pedal_cycles*0.91,
    substr(count_date, 6, 7) %in% q4 ~ pedal_cycles*1.06
    )
  )

# check each count point has 12 hours of data
ford = traffic_london_bam %>%
  group_by(year, count_date, count_point_id) %>%
  summarise(
    n_periods = n()
  )
ford %>%
  group_by(n_periods) %>%
  tally()

# remove those with the wrong number of hours
traffic_london_bam = left_join(traffic_london_bam, ford, by = c("year", "count_date", "count_point_id")) %>%
  filter(n_periods == 12)
dim(traffic_london_bam) #2096988

# check and remove rogue points
# ford[ford$n == 12,]
# traffic_london_bam[traffic_london_bam$count_point_id == 942684,]
# View(traffic_corrected[traffic_corrected$count_point_id == 942350,])

# traffic_london_bam = traffic_london_bam %>%
#   filter(count_point_id != 942684,
#          count_point_id != 942350)

summary(traffic_london_bam$pedal_cycles)

# Assign count points to 1km grid squares
traffic_london_bam = traffic_london_bam %>%
  mutate(grid_location = paste(signif(traffic_london_bam$easting, digits = 3),signif(traffic_london_bam$northing, digits = 3)))

dim(traffic_london_bam) #91446 #91560 #1037034
length(unique(traffic_london_bam$grid_location)) #1197

saveRDS(traffic_london_bam, "raw-dft-national-hourly.Rds")


#  Investigate count point numbers and placements per/across the year ----------------
traffic_london_days = traffic_london_bam %>%
  group_by(year, count_date, DoY, name, count_point_id, easting, northing, grid_location) %>%
  summarise(pedal_cycles_adj = sum(pedal_cycles_adj))
dim(traffic_london_days) #15260 #172839

# check one day per site per year
ford = traffic_london_days %>%
  group_by(year, count_point_id) %>%
  tally()
unique(ford$n)
# [1] 1

saveRDS(traffic_london_days, "raw-dft-national-daily.Rds")

#Get London Borough boundaries for maps
lads = readRDS("lads.Rds")
boroughs = as.character(spData::lnd$NAME)
lads = lads %>%
  filter(Name %in% boroughs)

traffic_london_days %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  # filter(year == 2011) %>%
  mapview() + mapview(lads)

traffic_london_days %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  filter(year == 2018) %>%
  mapview() + mapview(lads)

traffic_london_days %>%
  group_by(year) %>%
  count()



# sampling by day of year. It starts slowly in march and goes on to november, very few counts in final weeks. No counts in August.
ggdensity(traffic_london_days, x = "DoY",
          fill = "#0073C2FF", color = "#0073C2FF",
          rug = TRUE)
gghistogram(traffic_london_days, x = "DoY", bins = 314,
            fill = "#0073C2FF", color = "#0073C2FF",
            # rug = TRUE,
            xlab = "Day of Year",
            ylab = "Number of cycle counts")

# Get vector of days for which we have at least 10 counts
counts_per_day = traffic_london_days %>%
  group_by(DoY) %>%
  tally()
days_to_use = counts_per_day %>%
  filter(n >= 10)
days_to_use = days_to_use$DoY

# sampling by year - there were more counts in 2008 and 2009 than other years
gghistogram(traffic_london_days, x = "year",
            fill = "#0073C2FF", color = "#0073C2FF",
            bins = 19,
            rug = TRUE,
            xlab = "Year",
            ylab = "Number of cycle counts")

## plot density of sampling points across london spatially
grid_density = traffic_london_days %>%
  group_by(year, grid_location) %>%
  summarise(n = length(unique(count_point_id)))

# ggplot(traffic_london_days, aes(x = easting, y = northing)) +
#   geom_raster(aes(fill = grid_density$n)) +
#   # facet_wrap(~ year, ncol = 5) +
#   scale_fill_viridis(name = "Number of count points", option = 'plasma',
#                      na.value = 'transparent') +
#   coord_fixed(ratio = 1)

# Pre-process for borough model to remove multiple counts within the same grid square----------------
# this avoids the problem that nearby counts could be made on different dates

# sampleWithoutSurprises = function(x) {
#   if (length(x) <= 1) {
#     return(x)
#   } else {
#     return(sample(x,1))
#   }
# }


# randomise the row order of the data
set.seed(42)
new_order = sample(nrow(traffic_london_bam))
traffic_london_blue = traffic_london_bam[new_order, ]

# take the first record from each grid square
one_per_square = traffic_london_blue %>%
  distinct(year, name, grid_location, .keep_all = TRUE)




# Investigate annual means ------------------------------------------------

plot(pedal_cycles ~ year, data = one_per_square)
forplot = one_per_square %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
plot(pedal_cycles ~ year, data = forplot)

plot(pedal_cycles ~ year, data = traffic_london_bam)
forplot = traffic_london_bam %>%
  group_by(year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))
plot(pedal_cycles ~ year, data = forplot)




# Fit GAM model using national grid locations--------------------------------------------

M = list(c(1, 0.5), NA)

# m = bam(pedal_cycles ~
#             DoY + I(DoY^2) +
#             s(year) +
#             s(easting, northing, bs = 'ds', m = c(1, 0.5)) +
#             ti(easting, northing, year, d = c(2,1), bs = c('ds','tp'),
#                m = M),
#             family = nb(link = "log"),
#           data = traffic_london_bam, method = 'fREML',
#           nthreads = 4, discrete = TRUE)
# summary(m)
#
# plot(m$terms)
# termplot(m, terms = c("DoY", "I(DoY^2)"))

# i think the model should be negative binomial, so mean and variance don't need to be equal
# should i constrict the knots for the interaction term?
m = bam(pedal_cycles ~
           s(hour) + #added in term for hour of day
           s(DoY, bs = "cr", k = 3) + #smooth term with a low number of knots to prevent the few november counts from skewing the results
           s(year, k = 5) +
           s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5)) +
           ti(easting, northing, year, d = c(2,1), bs = c('ds','tp'),
               m = M, k = c(25, 5)) +
           ti(easting, northing, hour, d = c(2,1), bs = c('ds','tp'),
              m = M, k = c(25, 5)),
         family = nb(link = "log"),
         data = traffic_london_bam, method = 'fREML',
         nthreads = 4, discrete = TRUE)
summary(m)

m = bam(pedal_cycles ~
          s(DoY, bs = "cr", k = 3) + #smooth term with a low number of knots to prevent the few november counts from skewing the results
          s(year, k = 5) +
          s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5)) +
          ti(easting, northing, year, d = c(2,1), bs = c('ds','tp'),
             m = M, k = c(25, 5)),
        family = nb(link = "log"),
        data = traffic_london_bam, method = 'fREML',
        nthreads = 4, discrete = TRUE)

print(m)
m$family$getTheta(TRUE)

AIC(m,m3)

plot(m, pages = 1, scheme = 2, shade = TRUE, scale = 0)
plot(m, pages = 1, scheme = 2, shade = TRUE)

# check model fit
gam.check(m, pages = 1)
plot(traffic_london_bam$hour, residuals(m))
plot(traffic_london_bam$DoY, residuals(m))
plot(traffic_london_bam$year, residuals(m))
plot(traffic_london_bam$easting, residuals(m))
plot(traffic_london_bam$northing, residuals(m))



# Fit GAM model using random effect of borough ----------------------------

# m3 = gamm(pedal_cycles ~
#           s(DoY, bs = "cr", k = 3) + #smooth term with a low number of knots to prevent the few november counts from skewing the results
#           s(year, k = 5),
#           random = list(name=~1)
#          # + ti(name, year, d = c(1,1), bs = c('re','tp'))
#          ,
#         family = negbin(theta = 0.9415, link = "log"),
#         data = one_per_square, method = 'fREML')
# summary(m3)

# should i constrict the knots further for the year model terms? this might help prevent some outer boroughs from having reduced fitted values in the intermediate years - is this a model artefact or a real phenomenon?
# how do i deal with non-independence of the random effects, given the geographically close boroughs will be similar to one another?
m4 = bam(pedal_cycles ~
            s(DoY, bs = "cr", k = 3) + #smooth term with a low number of knots to prevent the few november counts from skewing the results
            s(year, k = 5) +
            s(name, bs = "re") +
            ti(name, year, d = c(1,1), bs = c('re','tp'), k = c(NA, 5)),
          family = nb(link = "log"),
          data = one_per_square, method = 'fREML')
summary(m4)



plot(m4, pages = 1, scheme = 2, shade = TRUE, scale = 0)
plot(m4, pages = 1, scheme = 2, shade = TRUE)

# check model fit
gam.check(m4, pages = 1)
plot(one_per_square$DoY, residuals(m4))
plot(one_per_square$year, residuals(m4))
plot(one_per_square$name, residuals(m4))

# rsd = residuals(m4,type="deviance")
# gam(rsd~s(year, k = 10)-1, data = one_per_square, select = TRUE)

##we want to maximise the proportion of the total deviance explained by the current model.
# The scale estimate for gaussian models is the residual standard error squared. We want to minimise this.
# GCV score is the generalised cross-validation score of the fitted GAM. A smaller score means a better fitted model (like AIC)



# Easting/northing model predictions --------------------------------------


## Make annual predictions while keeping DoY and hour constant

# assign the framework that will be used as a basis for predictions
pdata = with(traffic_london_bam,
              expand.grid(hour = 10, DoY = 150,
                          year = seq(min(year), max(year), by = 1),
                          easting = seq(signif(min(easting), digits = 3), signif(max(easting), digits = 3), by = 1000), # changed this to make regular 1km grid squares
                          northing  = seq(signif(min(northing), digits = 3), signif(max(northing), digits = 3), by = 1000)))
# make predictions according to the GAM model
fit_excl_DoYhour = predict(m, newdata = pdata, type = "response", exclude = c("DoY", "hour"), newdata.guaranteed = TRUE)
# predictions for points far from any counts set to NA
ind = exclude.too.far(pdata$easting, pdata$northing,
                       traffic_london_bam$easting, traffic_london_bam$northing, dist = 0.02)
fit_excl_DoYhour[ind] = NA
# join the predictions with the framework data
pred_all_points_year = cbind(pdata, Fitted = fit_excl_DoYhour)
pred_all_points_year = pred_all_points_year %>%
  drop_na()

saveRDS(pred_all_points_year, "predictions-by-year.Rds")

ggplot(pred_all_points_year, aes(x = easting, y = northing)) +
  geom_raster(aes(fill = Fitted)) + facet_wrap(~ year, ncol = 5) +
  scale_fill_viridis(name = "Pedal cycles", option = 'plasma',
                     na.value = 'transparent') +
  # coord_quickmap() +
  coord_fixed(ratio = 1) +
  # geom_line(lads, alpha(0.1)) +
  theme(legend.position = 'top', legend.key.width = unit(2, 'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

## make predictions by hour

pdata = with(traffic_london_bam,
             expand.grid(hour = seq(min(hour), max(hour), by = 1),
                         DoY = 150,
                         year = 2011,
                         easting = seq(signif(min(easting), digits = 3), signif(max(easting), digits = 3), by = 1000), # changed this to make regular 1km grid squares
                         northing  = seq(signif(min(northing), digits = 3), signif(max(northing), digits = 3), by = 1000)))
# make predictions according to the GAM model
fit_hour = predict(m, pdata, type = "response", exclude = c("DoY", "year"))
# predictions for points far from any counts set to NA
ind = exclude.too.far(pdata$easting, pdata$northing,
                      traffic_london_bam$easting, traffic_london_bam$northing, dist = 0.02)
fit_hour[ind] = NA
# join the predictions with the framework data
pred_all_points_hour = cbind(pdata, Fitted = fit_hour)
pred_all_points_hour = pred_all_points_hour %>%
  drop_na()

saveRDS(pred_all_points_hour, "predictions-by-hour.Rds")

## make predictions by DoY

pdata = with(traffic_london_bam,
             expand.grid(hour = 10,
                         DoY = days_to_use,
                         year = 2011,
                         easting = 530000,
                         northing  = 180000))
# make predictions according to the GAM model
fit_DoY = predict.gam(m, pdata, type = "response", exclude = c("year", "hour", "easting", "northing"))
# join the predictions with the framework data
pred_all_points_DoY = cbind(pdata, Fitted = fit_DoY)

# # `exclude` doesn't seem to be doing anything. Why is this?
# fit_DoY2 = predict.gam(m, pdata, type = "response")
# # join the predictions with the framework data
# pred_all_points_DoY2 = cbind(pdata, Fitted = fit_DoY2)
# identical(fit_DoY, fit_DoY2)

saveRDS(pred_all_points_DoY, "predictions-by-DoY.Rds")

# ## Make predictions for individual locations
# pdata = with(traffic_london_bam,
#               expand.grid(DoY = 170,
#                           year = seq(min(year), max(year), length = 500),
#                           easting = 540000,
#                           northing  = 188000))
# fit = data.frame(predict(m, type = "response", newdata = pdata, se.fit = TRUE))
# fit = transform(fit, upper = fit + (2 * se.fit), lower = fit - (2 * se.fit))
# pred = cbind(pdata, fit)
# ggplot(pred, aes(x = year, y = fit)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), fill = 'grey', alpha = 0.5) +
#   geom_line() +
#   labs(x = NULL, y = "Pedal cycles")



# Assign raster cells to boroughs -----------------------------------------

## Get borough geometry and join with pred_all_points
lads = readRDS("lads.Rds")
boroughs = as.character(spData::lnd$NAME)
lads = lads %>%
  filter(Name %in% boroughs)

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
  group_by(Name, year, hour, DoY) %>%
  summarise(borough_mean_cycles = mean(Fitted))
View(pb_preds_year)
saveRDS(pb_preds_year, "predictions-borough-year.Rds")

## Assign to borough for predictions by hour
pred_sf = pred_all_points_hour %>%
  st_as_sf(coords = c("easting", "northing"), crs = 27700)
point_to_borough = st_join(x = pred_sf, y = borough_geom)
## Calculate mean hourly predictions for each borough
pb_preds_hour = point_to_borough %>%
  drop_na() %>%
  group_by(Name, hour, year, DoY) %>%
  summarise(borough_mean_cycles = mean(Fitted))
View(pb_preds_hour)
saveRDS(pb_preds_hour, "predictions-borough-hour.Rds")

# Borough model predictions -----------------------------------------------

## Make predictions for individual boroughs
boroughs = as.character(spData::lnd$NAME)

pdata = with(one_per_square,
             expand.grid(
               # DoY = 170,
               DoY = days_to_use,
                         year = seq(min(year), max(year), length = 19), # can change length to a higher number for smoother graphs
                        name = "Southwark"))
fit = data.frame(predict(m4, type = "response", newdata = pdata, se.fit = TRUE))
fit = transform(fit, upper = fit + (2 * se.fit), lower = fit - (2 * se.fit)) # find 95% confidence interval
pred = cbind(pdata, fit)
ggplot(pred, aes(x = year, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = 'grey', alpha = 0.5) +
  geom_line() +
  labs(x = NULL, y = "Pedal cycles")

## Make predictions for all boroughs
boroughs = as.character(spData::lnd$NAME)

pdata = with(one_per_square,
             expand.grid(DoY = 170,
                         year = seq(min(year), max(year), length = 19), # can change length to a higher number for smoother graphs
                         name = boroughs))
fit = data.frame(predict(m4, type = "response", newdata = pdata, se.fit = TRUE))
fit = transform(fit, upper = fit + (2 * se.fit), lower = fit - (2 * se.fit)) # find 95% confidence interval
pred_all_boroughs = cbind(pdata, fit)
# (y axis has log10 scale)
ggplot(pred_all_boroughs, aes(x = year, y = fit)) +
  scale_y_log10() +
  facet_wrap(~ name, ncol = 5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = 'grey', alpha = 0.5) +
  geom_line() +
  labs(x = NULL, y = "Pedal cycles")

# View the predictions
pred_all_boroughs$fit

saveRDS(pred_all_boroughs, "borough-annual-predictions.Rds")





# Compare GAM predictions to the 2011 census data -------------------------

# there is no linear relationship between `km_cycled` and `fit` because `fit` is the (gam modelled) estimate of the average daily cycle count for points within a borough, while `km_cycled` depends on the length of roads within the borough (and is for peak hour commutes only)

# I multiply the model predicted counts by the total length of the PCT route network in each borough. This should be more a appropriate measure of exposure.
# The count points have a link_length_km field, which could be used instead to make calculations base on specific road lengths. But there are so many smaller and residential roads without count points that this is likely to further skew results towards the main roads (which are likely to have longer link lengths)

# find PCT route network length in each borough
cent = readRDS("cent.Rds")

c_london = st_join(lads, cent, by = st_within)

c_london = c_london %>%
  mutate(km_cycled = bicycle*lengths/1000)

borough_network_length = c_london %>%
  group_by(Name) %>%
  summarise(lengths = sum(lengths))

# join route network length with model predictions
pred_with_lengths = pred_all_boroughs %>%
  mutate(name = as.character(name)) %>%
  inner_join(borough_network_length, by = c("name" = "Name")) %>%
  mutate(fit_km = fit * lengths)

pred_2011 = pred_with_lengths %>%
  filter(year == 2011)


# join model results for 2011 with PCT data
rate_per_borough = read_rds("rate_per_borough.Rds") %>%
  st_drop_geometry()

dft_and_pct = inner_join(pred_2011, rate_per_borough, by = c("name" = "Name"))


plot(fit_km ~ km_cycled, data = dft_and_pct, log = "xy")
text(fit_km ~ km_cycled, data = dft_and_pct, labels = name, cex = 0.7, pos = 1)

cor(dft_and_pct$fit_km, dft_and_pct$km_cycled) #0.964

### same for raster model predictions

# join route network length with model predictions
pred_with_lengths_rast = pb_preds_year %>%
  st_drop_geometry() %>%
  mutate(Name = as.character(Name)) %>%
  inner_join(borough_network_length, by = "Name") %>%
  mutate(fit_km = borough_mean_cycles * lengths)

pred_2011_rast = pred_with_lengths_rast %>%
  filter(year == 2011)

# join model results for 2011 with PCT data
rate_per_borough = read_rds("rate_per_borough.Rds") %>%
  st_drop_geometry()

dft_and_pct_rast = inner_join(pred_2011_rast, rate_per_borough, by = "Name")


plot(fit_km ~ km_cycled, data = dft_and_pct_rast, log = "xy", ylab = "Grid-derived GAM prediction (km)", xlab = "2011 census cycle commutes (km)")
text(fit_km ~ km_cycled, data = dft_and_pct_rast, labels = Name, cex = 0.7, pos = 1)

cor(dft_and_pct_rast$fit_km, dft_and_pct_rast$km_cycled) #0.990
# cor was 0.980 in model without hour, and westminster was an outlier with lower fitted values. In model with hour this is no longer the case.


# Get adjustment factors to use with annual stats19 data -------------------

dft_and_pct_adjustment = dft_and_pct %>%
  mutate(adj_factor = km_cycled/fit_km) %>%
  select(name, adj_factor)

# Make annual adjustments to the 2011 census data -------------------------


adjust_join = pred_with_lengths %>%
  left_join(dft_and_pct_adjustment, by = "name") %>%
  mutate(km_cycled_estimate = adj_factor*fit_km)

View(adjust_join)

saveRDS(adjust_join, "km_cycled_adjustments.Rds")

# Compare GAM predictions to TfL cycle counts ------------------

tfl_counts = read_csv("tfl-counter-results-london-boroughs-2015-2019.csv")

ggplot(tfl_counts, aes(x = year, y = relative_to_2015)) +
  geom_line(aes(color = Borough))

dft_and_tfl = inner_join(pred_all_boroughs, tfl_counts, by = c("year", "name" = "Borough"))

plot(fit ~ mean_counts, data = dft_and_tfl, xlab = "TfL mean counts", ylab = "GAM model prediction from DfT counts", log = "xy")
text(fit ~ mean_counts, data = dft_and_tfl, labels = name, cex = 0.8)
text(fit ~ mean_counts, data = dft_and_tfl, labels = year, cex = 0.8)

cor(dft_and_tfl$fit, dft_and_tfl$mean_counts) #0.982

## use raster-based model predictions
dft_and_tfl_rast = inner_join(pb_preds, tfl_counts, by = c("year", "Name" = "Borough"))

plot(borough_mean_cycles ~ mean_counts, data = dft_and_tfl_rast, xlab = "TfL mean counts", ylab = "GAM model prediction from DfT counts", log = "xy")
text(borough_mean_cycles ~ mean_counts, data = dft_and_tfl_rast, labels = Name, cex = 0.8, log = "xy")
text(borough_mean_cycles ~ mean_counts, data = dft_and_tfl_rast, labels = year, cex = 0.8)

cor(dft_and_tfl_rast$borough_mean_cycles, dft_and_tfl_rast$mean_counts) #0.955


# Compare DfT raw counts (all of them) with TfL counts --------------------------

raw_borough_counts = traffic_london_bam %>%
  group_by(name, year) %>%
  summarise(pedal_cycles = mean(pedal_cycles))

dft_raw_and_tfl = inner_join(raw_borough_counts, tfl_counts, by = c("year", "name" = "Borough"))

plot(pedal_cycles ~ mean_counts, data = dft_raw_and_tfl, xlab = "TfL mean counts", ylab = "DfT mean raw counts")
text(pedal_cycles ~ mean_counts, data = dft_raw_and_tfl, labels = name, cex = 0.8)
text(pedal_cycles ~ mean_counts, data = dft_raw_and_tfl, labels = year, cex = 0.8)

cor(dft_raw_and_tfl$pedal_cycles, dft_raw_and_tfl$mean_counts) #0.860

# there are not many counts per borough per year (eg City of London 2018), and these can be skewed by date and choice of count location, so the model predictions seem better than the raw counts as a basis for comparisons with other data
# traffic_london_bam %>%
#   filter(year == 2018,
#          name == "City of London")

# Investigate Southwark counts --------------------------------------------

# DfT counts are very heavily skewed towards the northern (central London) fringe of Southwark
traffic_london_bam %>%
  filter(name == "Southwark") %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  mapview::mapview() +
lads %>%
  filter(Name == "Southwark") %>%
  mapview()


