library(tidyverse)
library(trafficalmr)

# rural drivers - who they hit
# young driver - age/passenger/other/where?

a = stats19::get_stats19(year = 2015:2019, type = "ac")
c = stats19::get_stats19(year = 2015:2019, type = "cas")
v = stats19::get_stats19(year = 2015:2019, type = "veh")

cj = inner_join(c, a)
nrow(c) == nrow(cj) # per crash dataset

v_per_ac = v %>%
  group_by(accident_index) %>%
  filter(vehicle_reference == max(vehicle_reference)) # replace with largest

cjv = inner_join(cj, v_per_ac, by = "accident_index")

View(cjv)

table(cjv$age_of_driver)
table(cjv$vehicle_type)

p = c(`Van*.+` = "Van", `Pedal cycle` = "Bicycle",
      `(M|m)otorcycle*.+|Elec*.+` = "Motorcycle",
      `Taxi*|Data*.+|Agri*.+|Ridden*.+|Mobility*.+|Tram*.+|(M|m)otorcycle*.+|Elec*.+` = "Other",
      `Bus*.+` = "Bus", `Bus|Minibus*.+|Other*.+` = "Other", `Goods*.+` = "HGV")
v$veh_type = tc_recode_vehicle_type(v$vehicle_type, p)
# dput(unique(cjv$veh_type))
l = c("Bicycle", "Car", "Other", "Van", "HGV")

v$vehicle = factor(v$veh_type, levels = l, ordered = TRUE)
max(v$vehicle)
summary(v$vehicle)

v_per_ac = v %>%
  group_by(accident_index) %>%
  filter(vehicle == max(vehicle))

cjv = inner_join(cj, v_per_ac, by = "accident_index")
table(cjv$vehicle)

cjv$cas_type = tc_recode_casualties(cjv$casualty_type)

cas_veh_table = table(cjv$cas_type, cjv$vehicle)
cvt_df = as.data.frame(cas_veh_table)
ggplot(cvt_df) +
  geom_bar(aes(Var2, Freq, fill = Var1), stat = "identity") +
  scale_fill_discrete("Casualty type") +
  xlab("Largest vehicle involved") +
  ylab("Number of casualties")

ggplot(cjv) +
  geom_bar(aes(Var2, Freq, fill = Var1), stat = "identity") +
  scale_fill_discrete("Casualty type") +
  xlab("Largest vehicle involved") +
  ylab("Number of casualties")


cjv_active = cvj %>%
  filter()
# focus on walking cycling

saveRDS(cjv, "code/tests/app-who-hit-who/cjv.Rds")
names(cjv)
