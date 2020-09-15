get_predictions = function{}

# Inputs
pred_hour = readRDS("predictions-by-hour.Rds")
pred_year = readRDS("predictions-by-year.Rds")
pred_DoY = readRDS("predictions-by-DoY.Rds")

existing_count = data.frame(DoY = 100, year = 2010, hour = c(9:12), easting = 531000, northing = 180000, bicycles = 20)
# View(existing_count)

desired_hour = c(7:18)
desired_DoY = 250
desired_year = 2016

# Get single parameter correction factors
pred_hour_cols = pred_hour %>%
  select(hour, easting, northing, Fitted)
to_correct_hour = inner_join(pred_hour_cols, existing_count, by = c("hour", "easting", "northing"))
correction_factor_hour = to_correct_hour$bicycles/to_correct_hour$Fitted
# Use a single average hourly correction factor instead of separate ones for each hour there is count data for
correction_factor_hour = mean(correction_factor_hour)

pred_DoY_cols = pred_DoY %>%
  select(DoY, Fitted)
to_correct_DoY = inner_join(pred_DoY_cols, existing_count, by = c("DoY"))
correction_factor_DoY = to_correct_DoY$bicycles/to_correct_DoY$Fitted
correction_factor_DoY = mean(correction_factor_DoY)

pred_year_cols = pred_year %>%
  select(year, easting, northing, Fitted)
to_correct_year = inner_join(pred_year_cols, existing_count, by = c("year", "easting", "northing"))
correction_factor_year = to_correct_year$bicycles/to_correct_year$Fitted
correction_factor_year = mean(correction_factor_year)

# criteria for which new predictions are desired
new_hour = pred_hour %>%
  filter(easting == existing_count$easting[1], northing == existing_count$northing[1],
         hour == desired_hour)

new_DoY = pred_DoY %>%
  filter(DoY == desired_DoY)

new_year = pred_year %>%
  filter(easting == existing_count$easting[1], northing == existing_count$northing[1],
         year == desired_year)

# get adjustments for each parameter
joined_hour = left_join(new_hour, existing_count, by = c("DoY", "year", "hour", "easting", "northing"))
joined_hour = joined_hour %>%
  mutate(predicted_bicycles = correction_factor_hour*Fitted)
# View(joined_hour)
hour_adjustment = joined_hour %>%
  mutate(predict_hour = predicted_bicycles/existing_count$bicycles) %>%
  select(hour, predict_hour)

joined_DoY = left_join(new_DoY, existing_count, by = c("DoY", "year", "hour", "easting", "northing"))
joined_DoY = joined_DoY %>%
  mutate(predicted_bicycles = correction_factor_DoY*Fitted)
# View(joined_DoY)
DoY_adjustment = joined_DoY %>%
  mutate(predict_DoY = predicted_bicycles/existing_count$bicycles) %>%
  select(DoY, predict_DoY)

joined_year = left_join(new_year, existing_count, by = c("DoY", "year", "hour", "easting", "northing"))
joined_year = joined_year %>%
  mutate(predicted_bicycles = correction_factor_year*Fitted)
# View(joined_year)
# year_adjustment = joined_year$predicted_bicycles/existing_count$bicycles
year_adjustment = joined_year %>%
  mutate(predict_year = predicted_bicycles/existing_count$bicycles) %>%
  select(year, predict_year)

# get the full new predictions
existing_hour = existing_count %>%
  select(hour, bicycles) %>%
  rename(count_hour = bicycles)

existing_DoY = existing_count %>%
  select(DoY, bicycles) %>%
  rename(count_DoY = bicycles)

existing_year = existing_count %>%
  select(year, bicycles) %>%
  rename(count_year = bicycles)

full_predictions = data.frame(hour = desired_hour, DoY = desired_DoY, year = desired_year) %>%
  inner_join(hour_adjustment, by = "hour") %>%
  inner_join(DoY_adjustment, by = "DoY") %>%
  inner_join(year_adjustment, by = "year") %>%
  left_join(existing_hour, by = "hour") %>%
  left_join(existing_DoY, by = "DoY") %>%
  left_join(existing_year, by = "year")
full_predictions$original = existing_count$bicycles

full_predictions = full_predictions %>%
  mutate(final_prediction = original * predict_hour * predict_DoY * predict_year) %>%
  select(hour, DoY, year, final_prediction)
full_predictions
# find the simple model predictions for this grid cell for comparison
pred_hour %>%
  filter(easting == existing_count$easting, northing == existing_count$northing)
