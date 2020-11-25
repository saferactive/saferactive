# This script gets DfT severity adjustment factors on a per-casualty basis and joins them with the stats19 casualty data
# The resulting output "casualties-adjusted.Rds" is uploaded as a release (tag = "v0.1") and is then used in the script `crash-data.R`

library(stats19)
library(tidyverse)

# Old method using manual download
# piggyback::pb_download("casualty-adjustment.csv")
# cas_adjust = read_csv("casualty-adjustment.csv")

# New method using stats19 function
cas_adjust = get_stats19_adjustments(data_dir = "severity-adjust", filename = "cas_adjustment_lookup_2019.csv")


# dim(cas_adjust) #3163331
cas_adjust = rename(cas_adjust, vehicle_reference = Vehicle_Reference,
                    casualty_reference = Casualty_Reference)

years = 2009:2019
casualties_all = get_stats19(year = years, type = "cas")
# dim(casualties_all) #2060935
# summary(casualties_all$accident_index %in% cas_adjust$accident_index)

adjust = left_join(casualties_all, cas_adjust, by = c("accident_index", "vehicle_reference", "casualty_reference")) %>%
  filter(casualty_severity != "Fatal")
# dim(adjust) #2040869

# For serious and slight casualties not listed in the adjustment data (these lines are no longer required with new adjustment data post-Sept 2020)
# adjust$Adjusted_Serious[adjust$casualty_severity == "Serious" & is.na(adjust$Adjusted_Serious)] = 1
# adjust$Adjusted_Serious[adjust$casualty_severity != "Serious" & is.na(adjust$Adjusted_Serious)] = 0
# adjust$Adjusted_Slight[adjust$casualty_severity == "Slight" & is.na(adjust$Adjusted_Slight)] = 1
# adjust$Adjusted_Slight[adjust$casualty_severity != "Slight" & is.na(adjust$Adjusted_Slight)] = 0

# Deal with the fatal casualties
casualties_fatal = casualties_all %>%
  filter(casualty_severity == "Fatal") %>%
  mutate(Adjusted_Serious = NA,
         Adjusted_Slight = NA,
         Injury_Based = NA)

casualties_adjusted = rbind(adjust, casualties_fatal)
# dim(casualties_adjusted) #2060935

write_rds(casualties_adjusted, "casualties_adjusted.Rds")
# piggyback::pb_upload("casualties_adjusted.Rds")


# get data from source - tests --------------------------------------------

# piggyback::pb_download("casualty-adjustment.csv")

adj_zip <- tempfile(fileext = ".zip")
download.file(
  url = "http://data.dft.gov.uk/road-accidents-safety-data/accident-and-casualty-adjustment-2004-to-2019.zip",
  destfile = adj_zip
)

download.file(
  url = "http://data.dft.gov.uk/road-accidents-safety-data/accident-and-casualty-adjustment-2004-to-2019.zip",
  destfile = adj_zip
)


unzip(adj_zip, exdir = tempdir())
list.files(tempdir(), full.names = TRUE)

cas_adjusted_from_zip = readr::read_csv("/tmp/RtmpgcjWM0/cas_adjustment_lookup_2019.csv")
nrow(cas_adjusted_from_zip)
head(cas_adjusted_from_zip)


# read-in adjustment figures
adj_figures <-read.csv(
  file = file.path(tempdir(), "accident_adjustment_lookup_2019.csv"),
  stringsAsFactors = FALSE,
  col.names = c("accident_index", "adjusted_serious", "adjusted_slight", "injury_based")
)

class(adj_figures$accident_index)
summary(adj_figures$adjusted_serious)
summary(adj_figures$adjusted_slight)
head(adj_figures)
nrow(adj_figures)
