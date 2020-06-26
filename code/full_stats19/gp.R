# read in the Rds
ow = setwd("ignore")
if(!file.exists("agg7918.Rds")) stop("get the RDS for 79 to 18")
all = readRDS("agg7918.Rds")

agg = aggregate(all$count, by=list(annual = all$year), FUN=length)

# get geography
ggplot(dat = all, aes(x = year, y = count)) + geom_line()
# vis!

#exit
setwd(ow)
