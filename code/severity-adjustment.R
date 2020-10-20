library(stats19)
library(tidyverse)

# piggyback::pb_download("casualty-adjustment.csv")

cas_adjust = read_csv("casualty-adjustment.csv")

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

# For serious and slight casualties not listed in the adjustment data
adjust$Adjusted_Serious[adjust$casualty_severity == "Serious" & is.na(adjust$Adjusted_Serious)] = 1
adjust$Adjusted_Serious[adjust$casualty_severity != "Serious" & is.na(adjust$Adjusted_Serious)] = 0
adjust$Adjusted_Slight[adjust$casualty_severity == "Slight" & is.na(adjust$Adjusted_Slight)] = 1
adjust$Adjusted_Slight[adjust$casualty_severity != "Slight" & is.na(adjust$Adjusted_Slight)] = 0

casualties_fatal = casualties_all %>%
  filter(casualty_severity == "Fatal") %>%
  mutate(Adjusted_Serious = NA,
         Adjusted_Slight = NA,
         Injury_Based = NA)

casualties_adjusted = rbind(adjust, casualties_fatal)
# dim(casualties_adjusted) #2060935

write_rds(casualties_adjusted, "casualties_adjusted.Rds")
# piggyback::pb_upload("casualties_adjusted.Rds")
