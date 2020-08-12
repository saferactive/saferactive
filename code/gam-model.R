# library(imputeTS)
# install.packages("imputeTS")
#
# trend = traffic_london_points %>%
#   group_by(yearmonth) %>%
#   summarise(pedal_cycles = mean(pedal_cycles))
# plot(pedal_cycles ~ yearmonth, data = trend, type = "n")
# lines(pedal_cycles ~ yearmonth, data = trend)
#
# library(seasonal)
# seasonaladjusted = seas(trend) # seasonalvector is ts object
#
# plot(seasonaladjusted) # plots both seasonally adjusted and unadjusted series
#
# out(seasonallyadjusted)


library('mgcv')
library('ggplot2')
library('viridis')
theme_set(theme_bw())



traffic_london = readRDS("traffic_london.Rds")


traffic_london_points = traffic_london %>%
  select(year, count_date, local_authority_name, count_point_id, direction_of_travel, easting, northing, pedal_cycles) %>%
  group_by(year, count_date, local_authority_name, count_point_id, direction_of_travel, easting, northing) %>%
  summarise(pedal_cycles = sum(pedal_cycles)) %>%
  ungroup()

traffic_london_bam = transform(traffic_london_points,
                       count_point_id = factor(count_point_id),
                       DoY = as.numeric(lubridate::yday(count_date)))

# Investigate count point placements per year

traffic_london_bam %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  filter(year == 2011) %>%
  mapview()

traffic_london_bam %>%
  sf::st_as_sf(coords = c("easting", "northing"), crs = 27700) %>%
  filter(year == 2018) %>%
  mapview()

traffic_london_bam %>%
  group_by(year) %>%
  count()

library(ggpubr)

# sampling by day of year. It starts slowly in march and goes on to november, very few counts in final weeks. No counts in August.
ggdensity(traffic_london_bam, x = "DoY",
          fill = "#0073C2FF", color = "#0073C2FF",
          rug = TRUE)
gghistogram(traffic_london_bam, x = "DoY", bins = 30,
            fill = "#0073C2FF", color = "#0073C2FF",
            rug = TRUE,
            xlab = "Day of Year",
            ylab = "Number of cycle counts")

# sampling by year - there were more counts in 2008 and 2009 than other years
gghistogram(traffic_london_bam, x = "year",
            fill = "#0073C2FF", color = "#0073C2FF",
            bins = 19,
            rug = TRUE,
            xlab = "Year",
            ylab = "Number of cycle counts")

## plot density of sampling points across london spatially


## Fit GAM model

M = list(c(1, 0.5), NA)

# m = bam(pedal_cycles ~
#             DoY + I(DoY^2) +
#             s(year) +
#             s(easting, northing, bs = 'ds', m = c(1, 0.5)) +
#             ti(easting, northing, year, d = c(2,1), bs = c('ds','tp'),
#                m = M),
#           data = traffic_london_bam, method = 'fREML',
#           nthreads = 4, discrete = TRUE)
# summary(m)
#
# plot(m$terms)
# termplot(m, terms = c("DoY", "I(DoY^2)"))

m4 = bam(pedal_cycles ~
           s(DoY, bs = "cr", k = 3) + #smooth term with a low number of knots to prevent the few november counts from skewing the results
           s(year) +
           s(easting, northing, bs = 'ds', m = c(1, 0.5)) +
           ti(easting, northing, year, d = c(2,1), bs = c('ds','tp'),
               m = M),
         data = traffic_london_bam, method = 'fREML',
         nthreads = 4, discrete = TRUE)
summary(m4)
AIC(m, m4)

plot(m4, pages = 1, scheme = 2, shade = TRUE, scale = 0)
plot(m4, pages = 1, scheme = 2, shade = TRUE)

# check model fit
gam.check(m4)
plot(traffic_london_bam$DoY, residuals(m4))
plot(traffic_london_bam$year, residuals(m4))

# rsd = residuals(m4,type="deviance")
# gam(rsd~s(year,k=10)-1,data=traffic_london_bam,select=TRUE)

##we want to maximise the proportion of the total deviance explained by the current model.
# The scale estimate for gaussian models is the residual standard error squared. We want to minimise this.
# GCV score is the generalised cross-validation score of the fitted GAM. A smaller score means a better fitted model (like AIC)



## Make annual predictions for a set DoY (late June)

# assign the framework that will be used as a basis for predictions
pdata = with(traffic_london_bam,
              expand.grid(DoY = 170,
                          year = seq(min(year), max(year), by = 1),
                          easting = seq(min(easting), max(easting), length = 100),
                          northing  = seq(min(northing), max(northing), length = 100)))
# make predictions according to the GAM model
fit = predict(m4, pdata)
# predictions for points far from any counts set to NA
ind = exclude.too.far(pdata$easting, pdata$northing,
                       traffic_london_bam$easting, traffic_london_bam$northing, dist = 0.02)
fit[ind] = NA
# join the predictions with the framework data
pred = cbind(pdata, Fitted = fit)

ggplot(pred, aes(x = easting, y = northing)) +
  geom_raster(aes(fill = Fitted)) + facet_wrap(~ year, ncol = 7) +
  scale_fill_viridis(name = "Pedal cycles", option = 'plasma',
                     na.value = 'transparent') +
  coord_quickmap() +
  theme(legend.position = 'top', legend.key.width = unit(2, 'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

