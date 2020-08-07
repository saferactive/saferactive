tfl-cycle-counter-code.R
================
robin
2020-08-07

``` r
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
```

    ## Parsed with column specification:
    ## cols(
    ##   `Survey wave (calendar quarter)` = col_character(),
    ##   `Equivalent financial quarter` = col_character(),
    ##   `Site ID` = col_character(),
    ##   Location = col_character(),
    ##   `Survey date` = col_datetime(format = ""),
    ##   Weather = col_character(),
    ##   Time = col_character(),
    ##   Period = col_character(),
    ##   Direction = col_character(),
    ##   `Start hour` = col_double(),
    ##   `Start minute` = col_double(),
    ##   `Number of normal cycles` = col_double(),
    ##   `Number of cycle hire bikes` = col_double(),
    ##   `Total cycles` = col_double(),
    ##   `Survey wave (year)` = col_logical(),
    ##   `Number of male cycles` = col_logical(),
    ##   `Number of female cycle` = col_logical(),
    ##   `Number of unknown cycle` = col_logical()
    ## )

    ## Warning: 753967 parsing failures.
    ##    row                col           expected actual                                       file
    ## 593473 Survey wave (year) 1/0/T/F/TRUE/FALSE   2015 'raw-tfl-cycle-counter-data-2014-2019.csv'
    ## 593474 Survey wave (year) 1/0/T/F/TRUE/FALSE   2015 'raw-tfl-cycle-counter-data-2014-2019.csv'
    ## 593475 Survey wave (year) 1/0/T/F/TRUE/FALSE   2015 'raw-tfl-cycle-counter-data-2014-2019.csv'
    ## 593476 Survey wave (year) 1/0/T/F/TRUE/FALSE   2015 'raw-tfl-cycle-counter-data-2014-2019.csv'
    ## 593477 Survey wave (year) 1/0/T/F/TRUE/FALSE   2015 'raw-tfl-cycle-counter-data-2014-2019.csv'
    ## ...... .................. .................. ...... ..........................................
    ## See problems(...) for more details.

``` r
counter_totals = counter_df %>%
  group_by(`Site ID`) %>%
  summarise(total = sum(`Total cycles`, na.rm = TRUE))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
nrow(counter_totals)
```

    ## [1] 1258

``` r
sum(counter_totals$total)
```

    ## [1] 12602653
