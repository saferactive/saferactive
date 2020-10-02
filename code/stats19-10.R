# Aim: get 2010:2019

library(stats19)
ac10 = get_stats19(year = 2010:2019)
nrow(ac10)
ca10 = get_stats19(year = 2010:2019, type = "casualties")
ve10 = get_stats19(year = 2010:2019, type = "vehicles")

saveRDS(ac10, "ac10.Rds")
saveRDS(ca10, "ca10.Rds")
saveRDS(ve10, "ve10.Rds")

piggyback::pb_upload("ac10.Rds")
piggyback::pb_upload("ca10.Rds")
piggyback::pb_upload("ve10.Rds")
