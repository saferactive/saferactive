tfl-cycle-counter-code.R
================
robin
2020-08-07

``` r
# Aim: get cycle counter data from TfL to estimate change over time in cycling

# To run this script and document the results...
# # setwd("code"); knitr::spin(hair = "tfl-cycle-counter-code.R"); setwd("..")
# rmarkdown::render("code/tfl-cycle-counter-code.R", output_format = "github_document")

base_url = "https://cycling.data.tfl.gov.uk/"
central_url = "https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Central%20London%20(area).xlsx"
inner_url = "https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Inner%20London%20(area).xlsx"
outer_url = "https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Outer%20London%20(area).xlsx"

cycle_counter_urls = c(central_url, inner_url, outer_url)
counter_df = purrr::map_df(cycle_counter_urls, readr::read_csv)
```

    ## Parsed with column specification:
    ## cols(
    ##   `PK` = col_character()
    ## )

    ## Warning: 137243 parsing failures.
    ## row            col  expected        actual                                                                                  file
    ##   2 NA             1 columns 2 columns     'https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Central%20London%20(area).xlsx'
    ##   3 NA             1 columns 2 columns     'https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Central%20London%20(area).xlsx'
    ##   4 NA             1 columns 2 columns     'https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Central%20London%20(area).xlsx'
    ##   5 NA             1 columns 3 columns     'https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Central%20London%20(area).xlsx'
    ##   6 PK           embedded null 'https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Central%20London%20(area).xlsx'
    ## ... .............. ......... ............. .....................................................................................
    ## See problems(...) for more details.

    ## Parsed with column specification:
    ## cols(
    ##   `PK` = col_character()
    ## )

    ## Warning: 108075 parsing failures.
    ## row col  expected    actual                                                                                file
    ##   2  -- 1 columns 2 columns 'https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Inner%20London%20(area).xlsx'
    ##   3  -- 1 columns 2 columns 'https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Inner%20London%20(area).xlsx'
    ##   4  -- 1 columns 2 columns 'https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Inner%20London%20(area).xlsx'
    ##   5  -- 1 columns 3 columns 'https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Inner%20London%20(area).xlsx'
    ##   7  -- 1 columns 2 columns 'https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Inner%20London%20(area).xlsx'
    ## ... ... ......... ......... ...................................................................................
    ## See problems(...) for more details.

    ## Parsed with column specification:
    ## cols(
    ##   `PK` = col_character()
    ## )

    ## Warning: 80684 parsing failures.
    ## row            col  expected        actual                                                                                file
    ##   2 NA             1 columns 2 columns     'https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Outer%20London%20(area).xlsx'
    ##   3 NA             1 columns 2 columns     'https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Outer%20London%20(area).xlsx'
    ##   4 NA             1 columns 2 columns     'https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Outer%20London%20(area).xlsx'
    ##   5 NA             1 columns 3 columns     'https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Outer%20London%20(area).xlsx'
    ##   6 PK           embedded null 'https://cycling.data.tfl.gov.uk/CycleCountsProgramme/Outer%20London%20(area).xlsx'
    ## ... .............. ......... ............. ...................................................................................
    ## See problems(...) for more details.

``` r
nrow(counter_df) # 340k counts!
```

    ## [1] 340979
