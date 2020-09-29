##Latest DfT dataset including 2019 counts
remotes::install_github("itsleeds/dftTrafficCounts")
library(dftTrafficCounts)

u = "http://data.dft.gov.uk/road-traffic/dft_traffic_counts_aadf.zip"
d = dtc_import(u = u)

saveRDS(d, "traffic-aadf-29092020.Rds")
traffic_aadf = readRDS("traffic-aadf-29092020.Rds")

# count points with at least 3 years of data 2009-2019?

#
