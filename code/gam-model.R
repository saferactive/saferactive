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

library(tidyverse)
library('mgcv')
library('ggplot2')
library('viridis')
theme_set(theme_bw())



traffic_london = readRDS("traffic_london.Rds")

# using bidirectional flows to match PCT, but since these are 7am-7pm this doesn't really reduce the increased residuals (+ve and -ve) at high predicted values (ie extreme variance in central london cycle flows)
traffic_london_points = traffic_london %>%
  select(year, count_date, local_authority_name, count_point_id, easting, northing, pedal_cycles) %>%
  group_by(year, count_date, local_authority_name, count_point_id, easting, northing) %>%
  summarise(pedal_cycles = sum(pedal_cycles)) %>%
  ungroup()

traffic_london_bam = transform(traffic_london_points,
                       count_point_id = factor(count_point_id),
                       local_authority_name = factor(local_authority_name),
                       DoY = as.numeric(lubridate::yday(count_date)))

summary(traffic_london_bam$pedal_cycles)

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
           s(DoY, bs = "cr", k = 3) + #smooth term with a low number of knots to prevent the few november counts from skewing the results
           s(year, k = 5) +
           s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5)) +
           ti(easting, northing, year, d = c(2,1), bs = c('ds','tp'),
               m = M),
         family = nb(link = "log"),
         data = traffic_london_bam, method = 'fREML',
         nthreads = 4, discrete = TRUE)
summary(m)
m2 = bam(pedal_cycles ~
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

AIC(m,m2)

# m2 = bam(pedal_cycles ~
#           s(DoY, bs = "cr", k = 3) + #smooth term with a low number of knots to prevent the few november counts from skewing the results
#           s(year, k = 5) +
#           s(easting, northing, k = 100, bs = 'ds', m = c(1, 0.5)) +
#           ti(easting, northing, year, d = c(2,1), bs = c('ds','tp'),
#              m = M),
#         family = poisson(link = "log"),
#         data = traffic_london_bam, method = 'fREML',
#         nthreads = 4, discrete = TRUE)

## attempt to get random effects of borough
# m3 = gamm(pedal_cycles ~
#           s(DoY, bs = "cr", k = 3) + #smooth term with a low number of knots to prevent the few november counts from skewing the results
#           s(year, k = 5),
#           random = list(local_authority_name=~1)
#          # + ti(local_authority_name, year, d = c(1,1), bs = c('re','tp'))
#          ,
#         family = negbin(theta = 0.9415, link = "log"),
#         data = traffic_london_bam, method = 'fREML')
# summary(m3)

# should i constrict the knots for the remaining model terms? can you do this for random effects? this might help prevent some outer boroughs from having reduced fitted values in the intermediate years - is this a model artefact or a real phenomenon?
# how do i deal with non-independence of the random effects, given the geographically close boroughs will be similar to one another?
m4 = bam(pedal_cycles ~
            s(DoY, bs = "cr", k = 3) + #smooth term with a low number of knots to prevent the few november counts from skewing the results
            s(year, k = 5) +
            s(local_authority_name, bs = "re") +
            ti(local_authority_name, year, d = c(1,1), bs = c('re','tp'), k = c(NA, 5)),
          family = nb(link = "log"),
          data = traffic_london_bam, method = 'fREML')
summary(m4)



plot(m, pages = 1, scheme = 2, shade = TRUE, scale = 0)
plot(m4, pages = 1, scheme = 2, shade = TRUE)

# check model fit
gam.check(m)
plot(traffic_london_bam$DoY, residuals(m4))
plot(traffic_london_bam$year, residuals(m4))
plot(traffic_london_bam$easting, residuals(m4))
plot(traffic_london_bam$northing, residuals(m4))

plot(traffic_london_bam$local_authority_name, residuals(m4))

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
                          easting = seq(min(easting), max(easting), length = 55),
                          northing  = seq(min(northing), max(northing), length = 42)))
# make predictions according to the GAM model
fit = predict(m2, pdata, type = "response")
# predictions for points far from any counts set to NA
ind = exclude.too.far(pdata$easting, pdata$northing,
                       traffic_london_bam$easting, traffic_london_bam$northing, dist = 0.02)
fit[ind] = NA
# join the predictions with the framework data
pred = cbind(pdata, Fitted = fit)

pred %>% filter(! is.na(Fitted)) %>% head()


# Do these four years at a time and add in london borough boundaries
ggplot(pred, aes(x = easting, y = northing)) +
  geom_raster(aes(fill = Fitted)) + facet_wrap(~ year, ncol = 5) +
  scale_fill_viridis(name = "Pedal cycles", option = 'plasma',
                     na.value = 'transparent') +
  coord_quickmap() +
  # geom_line(lads, alpha(0.1)) +
  theme(legend.position = 'top', legend.key.width = unit(2, 'cm'),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# ## Animated predictions
#
# p <- ggplot(pred, aes(x = easting, y = northing, frame = year)) +
#   geom_raster(aes(fill = Fitted)) +
#   scale_fill_viridis(name = "Pedal cycles", option = 'plasma',
#                      na.value = 'transparent') +
#   coord_quickmap() +
#   theme(legend.position = 'top', legend.key.width = unit(2, 'cm'))+
#   labs(x = 'easting', y = 'northing')
#
# gganimate(p, 'london.gif', interval = .2, ani.width = 500, ani.height = 800)

## Make predictions for individual locations
pdata = with(traffic_london_bam,
              expand.grid(DoY = 170,
                          year = seq(min(year), max(year), length = 500),
                          easting = 540000,
                          northing  = 188000))
fit <- data.frame(predict(m, type = "response", newdata = pdata, se.fit = TRUE))
fit <- transform(fit, upper = fit + (2 * se.fit), lower = fit - (2 * se.fit))
pred <- cbind(pdata, fit)
ggplot(pred, aes(x = year, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = 'grey', alpha = 0.5) +
  geom_line() +
  labs(x = NULL, y = "Pedal cycles")

## Make predictions for individual boroughs
boroughs = as.character(spData::lnd$NAME)

pdata = with(traffic_london_bam,
             expand.grid(DoY = 170,
                         year = seq(min(year), max(year), length = 19), # can change length to a higher number for smoother graphs
                        local_authority_name = "Southwark"))
fit <- data.frame(predict(m4, type = "response", newdata = pdata, se.fit = TRUE))
fit <- transform(fit, upper = fit + (2 * se.fit), lower = fit - (2 * se.fit)) # find 95% confidence interval
pred <- cbind(pdata, fit)
ggplot(pred, aes(x = year, y = fit)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = 'grey', alpha = 0.5) +
  geom_line() +
  labs(x = NULL, y = "Pedal cycles")

## Make predictions for all boroughs
boroughs = as.character(spData::lnd$NAME)

pdata = with(traffic_london_bam,
             expand.grid(DoY = 170,
                         year = seq(min(year), max(year), length = 19), # can change length to a higher number for smoother graphs
                         local_authority_name = boroughs))
fit2 <- data.frame(predict(m4, type = "response", newdata = pdata, se.fit = TRUE))
fit <- transform(fit, upper = fit + (2 * se.fit), lower = fit - (2 * se.fit)) # find 95% confidence interval
pred_all_boroughs <- cbind(pdata, fit)
# (y axis has log10 scale)
ggplot(pred_all_boroughs, aes(x = year, y = fit)) +
  scale_y_log10() +
  facet_wrap(~ local_authority_name, ncol = 5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = 'grey', alpha = 0.5) +
  geom_line() +
  labs(x = NULL, y = "Pedal cycles")

# View the predictions
pred$fit

saveRDS(pred_all_boroughs, "borough-annual-predictions.Rds")





# Compare GAM predictions to the 2011 census data -------------------------


rate_per_borough = read_rds("rate_per_borough.Rds")

pred_2011 = pred_all_boroughs %>%
  filter(year == 2011) %>%
  mutate(local_authority_name = as.character(local_authority_name))

# there is no linear relationship between `km_cycled` and `fit` because `fit` is the (gam modelled) estimate of the average daily cycle count for a borough, while `km_cycled` depends on the length of roads within the borough (and is for peak hour commutes only)
pred_join = inner_join(pred_2011, rate_per_borough, by = c("local_authority_name" = "Name"))
plot(fit ~ km_cycled, data = pred_join)
text(fit ~ km_cycled, data = pred_join, labels = local_authority_name, cex = 0.8)

pred_join = pred_join %>%
  mutate(adj_factor = km_cycled/fit) %>%
  select(local_authority_name, adj_factor)

# Make annual adjustments to the 2011 census data -------------------------


adjust_join = pred_all_boroughs %>%
  left_join(pred_join, by = "local_authority_name") %>%
  mutate(km_cycled_estimate = adj_factor*fit)

View(adjust_join)



# Compare GAM predictions to TfL cycle count predictions ------------------


