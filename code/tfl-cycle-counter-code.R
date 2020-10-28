# Aim: get cycle counter data from TfL to estimate change over time in cycling

# To run this script and document the results...
# rmarkdown::render("code/tfl-cycle-counter-code.R", output_format = "github_document", knit_root_dir = "..", output_dir = "code")


# get raw counter data ----------------------------------------------------

# base_url = "https://cycling.data.tfl.gov.uk/"
# central_url = "https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Central%20London%20(area).xlsx"
# inner_url = "https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Inner%20London%20(area).xlsx"
# outer_url = "https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Outer%20London%20(area).xlsx"
#
# dir.create("raw-tfl-cycle-counter-data")
# cycle_counter_urls = c(central_url, inner_url, outer_url)
# cycle_counter_names = file.path("raw-tfl-cycle-counter-data", basename(cycle_counter_urls))
# download.file(url = cycle_counter_urls, destfile = cycle_counter_names)
# counter_df = purrr::map_df(cycle_counter_names, readxl::read_excel, sheet = 2)
# nrow(counter_df) # 1.3m counts!
# counter_df
# summary(counter_df$`Survey date`)
# readr::write_csv(counter_df, "raw-tfl-cycle-counter-data-2014-2019.csv")
# piggyback::pb_upload("raw-tfl-cycle-counter-data-2014-2019.csv", repo = "itsleeds/saferroadsmap") # 174 MB

# get counter locations ---------------------------------------------------

# download.file(
#   "https://cycling.data.tfl.gov.uk/CycleCountsProgramme/X%20-%20Count%20sites%20list.xlsx",
#   "raw-tfl-cycle-counter-data/X%20-%20Count%20sites%20list.xlsx"
#   )
# cycle_counter_locations = readxl::read_excel("raw-tfl-cycle-counter-data/X%20-%20Count%20sites%20list.xlsx")
# summary(cycle_counter_locations)
# counter_locations = sf::st_as_sf(
#   cycle_counter_locations %>% select(UnqID, ProgID, SurveyDescription, Easting, Northing, Borough),
#   coords = c("Easting", "Northing"),
#   crs = 27700
#   )
# counter_locations = sf::st_transform(counter_locations, 4326)
# plot(counter_locations)
# sf::write_sf(counter_locations, "tfl-cycle-counter-locations.geojson")
# piggyback::pb_upload("tfl-cycle-counter-locations.geojson", repo = "itsleeds/saferroadsmap")

# exploratory data analysis -----------------------------------------------

library(tidyverse)
counter_locations = sf::read_sf("tfl-cycle-counter-locations.geojson")
counter_locations = counter_locations %>% rename(`Site ID` = UnqID)
counter_df = readr::read_csv("raw-tfl-cycle-counter-data-2014-2019.csv")
counter_totals = counter_df %>%
  group_by(`Site ID`) %>%
  summarise(total = sum(`Total cycles`, na.rm = TRUE))
nrow(counter_totals)
sum(counter_totals$total)

counter_totals = right_join(counter_locations, counter_totals)
plot(counter_totals["total"])

# get average counts per year per LA
summary(counter_df$`Survey wave (year)`)
summary(counter_df$`Number of normal cycles`)
counter_df$year = lubridate::year(counter_df$`Survey date`)
counter_df = left_join(counter_df, sf::st_drop_geometry(counter_locations %>% select(`Site ID`, Borough)))
table(counter_df$year)
table(counter_df$Borough)
counter_means_year = counter_df %>%
  group_by(year, Borough) %>%
  summarise(
    n_counters = length(unique(`Site ID`)),
    total_counts = sum(`Total cycles`, na.rm = TRUE),
    mean_counts = mean(`Total cycles`, na.rm = TRUE)
  )

plot(counter_means_year$year, counter_means_year$n_counters)
counter_means_year %>%
  group_by(Borough) %>%
  summarise(mean_counts = mean(mean_counts)) %>%
  arrange(desc(mean_counts))

ggplot(counter_means_year) +
  geom_line(aes(year, mean_counts, colour = Borough))

counter_means_2015 = counter_means_year %>%
  filter(year == 2015) %>%
  ungroup() %>%
  mutate(mean_counts_2015 = mean_counts) %>%
  select(Borough, mean_counts_2015)

counter_la_results = inner_join(counter_means_year, counter_means_2015) %>%
  mutate(relative_to_2015 = mean_counts / mean_counts_2015) %>%
  filter(year > 2014)

ggplot(counter_la_results) +
  geom_line(aes(year, relative_to_2015, colour = Borough))

readr::write_csv(counter_la_results, "tfl-counter-results-london-boroughs-2015-2019.csv")
piggyback::pb_upload("tfl-counter-results-london-boroughs-2015-2019.csv", repo = "itsleeds/saferroadsmap")

lads = spData::lnd %>% rename(Borough = NAME) %>%
  mutate(Borough = as.character(Borough)) %>%
  mutate(Name = abbreviate(Borough, minlength = 2))
lads$Borough[!lads$Borough %in% counter_la_results$Borough]
counter_means_2015$Borough[!counter_means_2015$Borough %in% lads$Borough]
lads = lads %>%
  mutate(Borough = str_replace(string = Borough, pattern = " and", replacement = " &"))
lads$Borough[!lads$Borough %in% counter_la_results$Borough]
lads_data = inner_join(lads, counter_la_results)

library(tmap)
tm_shape(lads_data) +
  tm_polygons("relative_to_2015", palette = "BrBG", n = 6) +
  tm_text(text = "Name", size = 0.7) +
  tm_facets("year")

# bonus: animated map:
# m = tm_shape(lads_data) +
#   tm_polygons("relative_to_2015", palette = "BrBG", n = 6) +
#   tm_text(text = "Name", size = 0.7) +
#   tm_facets(along = "year")
# tmap_animation(m, filename = "london-counter-results-tfl.gif", loop = FALSE, delay = 100)
# browseURL("london-counter-results-tfl.gif")


# Verification of GAM predictions -----------------------------------------


gam_preds = readRDS("pred-borough-change-cycles.Rds")

counter_la_results$Borough = gsub("&", "and", counter_la_results$Borough)

verify = left_join(counter_la_results, gam_preds, by = c("year", "Borough" = "Name"))
verify = verify %>%
  filter(year != 2015)
dim(verify)
cor(verify$relative_to_2015, verify$pred_relative_to_2015)^2 #R squared = 0.00189
plot(verify$relative_to_2015, verify$pred_relative_to_2015)
