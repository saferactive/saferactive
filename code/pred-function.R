get_predictions = function{}

# Inputs
pred_hour = readRDS("predictions-by-hour.Rds")
pred_year = readRDS("predictions-by-year.Rds")
pred_DoY = readRDS("predictions-by-DoY.Rds")

existing_count = data.frame(DoY = 150, year = 2015, hour = 12, easting = 531000, northing = 180000, bicycles = 5)
# View(existing_count)

desired_hour = c(7:18)
desired_DoY = 250
desired_year = 2016

# Get single parameter correction factors
pred_hour_cols = pred_hour %>%
  select(hour, easting, northing, Fitted)
to_correct_hour = inner_join(pred_hour_cols, existing_count, by = c("hour", "easting", "northing"))
correction_factor_hour = to_correct_hour$bicycles/to_correct_hour$Fitted

pred_DoY_cols = pred_DoY %>%
  select(DoY, Fitted)
to_correct_DoY = inner_join(pred_DoY_cols, existing_count, by = c("DoY"))
correction_factor_DoY = to_correct_DoY$bicycles/to_correct_DoY$Fitted

pred_year_cols = pred_year %>%
  select(year, easting, northing, Fitted)
to_correct_year = inner_join(pred_year_cols, existing_count, by = c("year", "easting", "northing"))
correction_factor_year = to_correct_year$bicycles/to_correct_year$Fitted

# criteria for which new predictions are desired
new_hour = pred_hour %>%
  filter(easting == existing_count$easting, northing == existing_count$northing,
         hour == desired_hour)

new_DoY = pred_DoY %>%
  filter(DoY == desired_DoY)

new_year = pred_year %>%
  filter(easting == existing_count$easting, northing == existing_count$northing,
         year == desired_year)

# get adjustments for each parameter
joined_hour = left_join(new_hour, existing_count, by = c("DoY", "year", "hour", "easting", "northing"))
joined_hour = joined_hour %>%
  mutate(predicted_bicycles = correction_factor_hour*Fitted)
# View(joined_hour)
hour_adjustment = joined_hour %>%
  mutate(predicted_bicycles = predicted_bicycles/existing_count$bicycles) %>%
  select(hour, predicted_bicycles)

joined_DoY = left_join(new_DoY, existing_count, by = c("DoY", "year", "hour", "easting", "northing"))
joined_DoY = joined_DoY %>%
  mutate(predicted_bicycles = correction_factor_DoY*Fitted)
# View(joined_DoY)
DoY_adjustment = joined_DoY %>%
  mutate(predicted_bicycles = predicted_bicycles/existing_count$bicycles) %>%
  select(DoY, predicted_bicycles)

joined_year = left_join(new_year, existing_count, by = c("DoY", "year", "hour", "easting", "northing"))
joined_year = joined_year %>%
  mutate(predicted_bicycles = correction_factor_year*Fitted)
# View(joined_year)
# year_adjustment = joined_year$predicted_bicycles/existing_count$bicycles
year_adjustment = joined_year %>%
  mutate(predicted_bicycles = predicted_bicycles/existing_count$bicycles) %>%
  select(year, predicted_bicycles)

# get the full new predictions
full_predictions = data.frame(hour = desired_hour, DoY = desired_DoY, year = desired_year, predicted_bicycles = NA)

existing_hour = existing_count %>%
  select(hour, bicycles) %>%
  rename(count_hour = bicycles)

existing_DoY = existing_count %>%
  select(DoY, bicycles) %>%
  rename(count_DoY = bicycles)

left_join(full_predictions, existing_hour, by = c("hour"))
left_join(full_predictions, existing_DoY, by = c("DoY"))


full_predictions$predicted_bicycles =


# joined %>%
#   filter(DoY == 150, year == 2011, hour == 10, easting == 531000, northing == 180000) %>%
#   View()
# View(joined)
