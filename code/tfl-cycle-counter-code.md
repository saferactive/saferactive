tfl-cycle-counter-code.R
================
robin
2020-08-07

``` r
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

``` r
counter_totals = right_join(counter_locations, counter_totals)
```

    ## Joining, by = "Site ID"

``` r
plot(counter_totals["total"])
```

![](/mnt/57982e2a-2874-4246-a6fe-115c199bc6bd/orgs/saferactive/saferactive/code/tfl-cycle-counter-code_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
# get average counts per year per LA
summary(counter_df$`Survey wave (year)`)
```

    ##    Mode    NA's 
    ## logical 1264064

``` r
summary(counter_df$`Number of normal cycles`)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.00    1.00    4.00   11.29   11.00 1177.00  288513

``` r
counter_df$year = lubridate::year(counter_df$`Survey date`)
counter_df = left_join(counter_df, sf::st_drop_geometry(counter_locations %>% select(`Site ID`, Borough)))
```

    ## Joining, by = "Site ID"

``` r
table(counter_df$year)
```

    ## 
    ##   2014   2015   2016   2017   2018   2019 
    ## 102540 234932 235264 232876 235604 212912

``` r
table(counter_df$Borough)
```

    ## 
    ##   Barking & Dagenham               Barnet               Bexley                Brent              Bromley               Camden       City of London              Croydon               Ealing 
    ##                 7680                12800                12800                16000                21120               125568               100224                15360                23040 
    ##              Enfield            Greenwich              Hackney Hammersmith & Fulham             Haringey               Harrow             Havering           Hillingdon             Hounslow 
    ##                12800                15360                36224                27520                14720                11392                11520                22400                12800 
    ##            Islington Kensington & Chelsea Kingston upon Thames              Lambeth             Lewisham               Merton               Newham            Redbridge Richmond upon Thames 
    ##                77440                30720                 9600               109952                23040                24320                14720                11520                18560 
    ##            Southwark               Sutton        Tower Hamlets       Waltham Forest           Wandsworth          Westminster 
    ##               108800                12800                38784                12800                46720               224960

``` r
counter_means_year = counter_df %>%
  group_by(year, Borough) %>%
  summarise(
    n_counters = length(unique(`Site ID`)),
    total_counts = sum(`Total cycles`, na.rm = TRUE),
    mean_counts = mean(`Total cycles`, na.rm = TRUE)
  )
```

    ## `summarise()` regrouping output by 'year' (override with `.groups` argument)

``` r
plot(counter_means_year$year, counter_means_year$n_counters)
```

![](/mnt/57982e2a-2874-4246-a6fe-115c199bc6bd/orgs/saferactive/saferactive/code/tfl-cycle-counter-code_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
counter_means_year %>%
  group_by(Borough) %>%
  summarise(mean_counts = mean(mean_counts)) %>%
  arrange(desc(mean_counts))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 33 x 2
    ##    Borough              mean_counts
    ##    <chr>                      <dbl>
    ##  1 City of London             23.2 
    ##  2 Islington                  14.8 
    ##  3 Southwark                  14.4 
    ##  4 Camden                     13.8 
    ##  5 Westminster                13.2 
    ##  6 Lambeth                    11.6 
    ##  7 Kensington & Chelsea        9.67
    ##  8 Hackney                     8.00
    ##  9 Hammersmith & Fulham        6.37
    ## 10 Tower Hamlets               6.13
    ## # … with 23 more rows

``` r
ggplot(counter_means_year) +
  geom_line(aes(year, mean_counts, colour = Borough))
```

    ## Warning: Removed 18 row(s) containing missing values (geom_path).

![](/mnt/57982e2a-2874-4246-a6fe-115c199bc6bd/orgs/saferactive/saferactive/code/tfl-cycle-counter-code_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
counter_means_2015 = counter_means_year %>%
  filter(year == 2015) %>%
  ungroup() %>%
  mutate(mean_counts_2015 = mean_counts) %>%
  select(Borough, mean_counts_2015)

counter_la_results = inner_join(counter_means_year, counter_means_2015) %>%
  mutate(relative_to_2015 = mean_counts / mean_counts_2015) %>%
  filter(year > 2014)
```

    ## Joining, by = "Borough"

``` r
ggplot(counter_la_results) +
  geom_line(aes(year, relative_to_2015, colour = Borough))
```

![](/mnt/57982e2a-2874-4246-a6fe-115c199bc6bd/orgs/saferactive/saferactive/code/tfl-cycle-counter-code_files/figure-gfm/unnamed-chunk-1-4.png)<!-- -->

``` r
readr::write_csv(counter_la_results, "tfl-counter-results-london-boroughs-2015-2019.csv")
piggyback::pb_upload("tfl-counter-results-london-boroughs-2015-2019.csv", repo = "itsleeds/saferroadsmap")
```

    ## uploading tfl-counter-results-london-boroughs-2015-2019.csv ...

    ##   |                                                                                                                                                                                                            |                                                                                                                                                                                                    |   0%  |                                                                                                                                                                                                            |====================================================================================================================================================================================================| 100%

``` r
lads = spData::lnd %>% rename(Borough = NAME) %>%
  mutate(Borough = as.character(Borough)) %>%
  mutate(Name = abbreviate(Borough, minlength = 2))
lads$Borough[!lads$Borough %in% counter_la_results$Borough]
```

    ## [1] "Hammersmith and Fulham" "Kensington and Chelsea" "Barking and Dagenham"

``` r
counter_means_2015$Borough[!counter_means_2015$Borough %in% lads$Borough]
```

    ## [1] "Barking & Dagenham"   "Hammersmith & Fulham" "Kensington & Chelsea"

``` r
lads = lads %>%
  mutate(Borough = str_replace(string = Borough, pattern = " and", replacement = " &"))
lads$Borough[!lads$Borough %in% counter_la_results$Borough]
```

    ## character(0)

``` r
lads_data = inner_join(lads, counter_la_results)
```

    ## Joining, by = "Borough"

``` r
library(tmap)
tm_shape(lads_data) +
  tm_polygons("relative_to_2015", palette = "BrBG", n = 6) +
  tm_text(text = "Name", size = 0.7) +
  tm_facets("year")
```

![](/mnt/57982e2a-2874-4246-a6fe-115c199bc6bd/orgs/saferactive/saferactive/code/tfl-cycle-counter-code_files/figure-gfm/unnamed-chunk-1-5.png)<!-- -->

``` r
# bonus: animated map:
# m = tm_shape(lads_data) +
#   tm_polygons("relative_to_2015", palette = "BrBG", n = 6) +
#   tm_text(text = "Name", size = 0.7) +
#   tm_facets(along = "year")
# tmap_animation(m, filename = "london-counter-results-tfl.gif", loop = FALSE, delay = 100)
# browseURL("london-counter-results-tfl.gif")
```
