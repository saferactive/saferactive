if(!dir.exists("ignored")) dir.create("ignored")
ow = setwd("ignored")

library(stats19)

#### last step!
acc7904 = readRDS("agg-y-s-l-7904.Rds")
acc0518 = readRDS("agg-y-s-l-05-18.Rds")
names(acc7904)
names(acc0518)
colnames(acc0518) = names(acc7904)
acc7918 = rbind(acc7904, acc0518)
saveRDS(acc7918, "agg7918.Rds")
###################wip############
acc = get_stats19(year = 2005)
# veh = get_stats19(year = 2005, type = "veh")
# cas = get_stats19(year = 2005, type = "cas")
# all = merge(merge(acc, veh), cas)

# collapse to year
acc$date
acc$date = as.Date(acc$date, "%Y-%m-%d")
acc$year = format(acc$date, "%Y")

# agg on year, severity and location
agg = aggregate(accident_index ~ year + accident_severity + local_authority_district,
                data = acc, FUN = length)
saveRDS(agg, "agg-y-s-l-05-14.Rds")
# head(agg)
# 3*416
acc = get_stats19(year = 2015:2018)
# acc_19 = get_stats19(year = 2019)
# errors
agg_1518 = aggregate(accident_index ~ accident_severity + local_authority_district,
                     data = acc, FUN = length)
agg_0518 = merge(agg, agg_1518, all.x = TRUE,
                 by = c('accident_severity', 'local_authority_district'))
agg_0518$accident_index = agg_0518$accident_index.x + agg_0518$accident_index.y
agg_0518 = agg_0518[, c('accident_severity', 'local_authority_district', 'accident_index')]
saveRDS(agg_0518, "~/code/saferactive/saferactive/ignored/agg_0518.Rds")
library(sf)
g = st_read("~/Downloads/ltlas.geojson")
summary(agg$local_authority_district %in% g$lad19nm)
n = agg$local_authority_district %in% g$lad19nm

# cleanup
agg_clean = agg_0518[n,]
agg_clean_sf = st_as_sf(agg_clean, st_geometry(g[n,]))
colnames(agg_clean_sf) = c("s", "l", "c", "geom")
st_geometry(agg_clean_sf) = "geom"
## test
plot(agg_clean_sf[, "c"])

####################### 79 -04
########################### 1.8gb
acc = read.csv("Stats19-Data1979-2004/Accidents7904.csv")
# cas = read.csv("Stats19-Data1979-2004/Casualty7904.csv")
# veh = read.csv("Stats19-Data1979-2004/Vehicles7904.csv")

# matching merge column
cols = colnames(acc)
cols[1] = "Acc_Index"
colnames(acc) = cols
# if(!(colnames(acc)[1] == colnames(veh)[1] && colnames(cas)[1] == colnames(acc)[1])) stop("merging index error")

# merge
# all = merge(merge(acc, veh), cas)
acc$Date = as.Date(acc$Date, "%d/%m/%Y")
acc$Year = format(acc$Date,"%Y")
# saveRDS(all, "Stats19-Data1979-2004/all-cols.Rds")
# names(all)
agg = aggregate(Acc_Index ~ Year + Accident_Severity + Local_Authority_.District., data = acc, FUN = length)
colnames(agg) = c("year", "accident_severity", "local_authority_district", "count")

# decode local authorities
lacodes = readxl::read_xls("Stats19-Data1979-2004/Road-Accident-Safety-Data-Guide-1979-2004.xls", 6)

# matching values
# agglacodes = unique(agg$local_authority_district)
# length(agglacodes)
# length(lacodes$code)
# length(all(agg$local_authority_district %in% lacodes$code)) == length(agg$local_authority_district)
# length(agglacodes) == length(lacodes$code)
# summary(is.na())
# remove data with no matching label
# class(agg$local_authority_district)
# class(lacodes$code)
# summary(agglacodes %in% lacodes$code)
agg = agg[agg$local_authority_district %in% lacodes$code, ]
agg$local_authority_district = lacodes$label[match(agg$local_authority_district, lacodes$code)]
agg
saveRDS(agg, "agg-y-s-l-7904.Rds")
# exit
setwd(ow)
