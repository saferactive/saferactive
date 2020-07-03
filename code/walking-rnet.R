# Aim: create walking route networks from PCT data


# test on small region ----------------------------------------------------

region_name = "isle-of-wight"
r = pct::get_pct_routes_fast(region = region_name, geography = "lsoa")
rnet_walking = stplanr::overline(r, attrib = "foot")
plot(rnet_walking)

# for london --------------------------------------------------------------

# region_name = "isle-of-wight"
# r = pct::get_pct_routes_fast(region = region_name, geography = "lsoa")
# rnet_walking = stplanr::overline(r, attrib = "foot")
# plot(rnet_walking)
# f = paste0("rnet_walking_", region_name, ".Rds")
# saveRDS(rnet_walking, f)
# piggyback::pb_upload(f)
# piggyback::pb_download_url(f)

# to run this from bash and report results:
# knitr::spin("code/walking-rnet.R")
# file.rename("walking-rnet.md", "code/walking-rnet.md")
