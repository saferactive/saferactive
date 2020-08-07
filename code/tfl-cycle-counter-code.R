# Aim: get cycle counter data from TfL to estimate change over time in cycling

# To run this script and document the results...
# # setwd("code"); knitr::spin(hair = "tfl-cycle-counter-code.R"); setwd("..")
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

# exploratory data analysis -----------------------------------------------

library(tidyverse)
counter_df = readr::read_csv("raw-tfl-cycle-counter-data-2014-2019.csv")
counter_totals = counter_df %>%
  group_by(`Site ID`) %>%
  summarise(total = sum(`Total cycles`, na.rm = TRUE))
nrow(counter_totals)
sum(counter_totals$total)
