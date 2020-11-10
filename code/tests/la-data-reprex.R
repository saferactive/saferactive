library(sf)

uf = "https://github.com/saferactive/saferactive/releases/download/0.1.1/counties_uas_gb_2019_bfc.Rds"
ub = "https://github.com/saferactive/saferactive/releases/download/0.1.1/counties_uas_gb_2019_ubc.Rds"
ff = basename(uf)
fb = basename(ub)


if(!file.exists(ff)) {
  download.file(uf, ff)
  download.file(ub, fb)
}

# high res version
upper_tier_high_res = readRDS(ff)
nrow(upper_tier_high_res)
pryr::object_size(upper_tier_high_res)
leeds_high_tier = upper_tier_high_res[upper_tier_high_res$ctyua19nm == "Leeds", ]
plot(leeds_high_tier$geometry)

# low res version
upper_tier_low_res = readRDS(fb)
nrow(upper_tier_low_res)
plot(upper_tier_low_res$geometry)
pryr::object_size(upper_tier_low_res)
leeds_low_tier = upper_tier_low_res[upper_tier_low_res$ctyua19nm == "Leeds", ]
plot(leeds_low_tier$geometry)
