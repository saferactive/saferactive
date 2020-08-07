# Aim: get cycle counter data from TfL to estimate change over time in cycling

# To run this script and document the results...
# # setwd("code"); knitr::spin(hair = "tfl-cycle-counter-code.R"); setwd("..")
# rmarkdown::render("code/tfl-cycle-counter-code.R", output_file = "tfl-cycle-counter-code.md")

base_url = "https://cycling.data.tfl.gov.uk/"
central_url = "https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Central%20London%20(area).xlsx"
inner_url = "https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Inner%20London%20(area).xlsx"
outer_url = "https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Outer%20London%20(area).xlsx"

cycle_counter_urls = c(central_url, inner_url, outer_url)
counter_df = purrr::map_df(cycle_counter_urls, readr::read_csv)
nrow(counter_df) # 340k counts!
