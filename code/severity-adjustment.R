library(stats19)
library(dplyr)

# piggyback::pb_download("casualty-adjustment.csv")

cas_adjust = read_csv("casualty-adjustment.csv")

# dim(cas_adjust) #3163331
cas_adjust = rename(cas_adjust, vehicle_reference = Vehicle_Reference,
                    casualty_reference = Casualty_Reference)

years = 2009:2018
casualties_all = get_stats19(year = years, type = "cas")
# dim(casualties_all) #1907777


adjust = inner_join(casualties_all, cas_adjust, by = c("accident_index", "vehicle_reference", "casualty_reference"))
# dim(adjust) #1889463

casualties_fatal = casualties_all %>%
  filter(casualty_severity == "Fatal") %>%
  mutate(Adjusted_Serious = NA,
         Adjusted_Slight = NA,
         Injury_Based = NA)

casualties_adjusted = rbind(adjust, casualties_fatal)
# dim(casualties_adjusted) #1907777

write_rds(casualties_adjusted, "casualties_adjusted.Rds")
# piggyback::pb_upload("casualties_adjusted.Rds")
