ksi = bounds %>%
  select(lad_name, mean_risk, diff_risk,
         diff_cycle_wd, diff_cycle,
         mean_km_cycled, la_code) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  sf::st_drop_geometry()

readr::write_csv(ksi, "../tgve/ksi.csv")

## police force
ksi.pf = pf_geom %>% select(pfa18nm, mean_risk, diff_risk,
                          diff_cycle, mean_km_cycled,
                          mean_km_percap) %>%
  rename(police_force = pfa18nm) %>%
  mutate_if(is.numeric, round, digits = 2)

pf.geojson = ksi.pf %>% select(police_force)
sf::write_sf(pf.geojson, "../tgve/pf-only-name.geojson")
ksi.pf %>% sf::st_drop_geometry() %>%
  readr::write_csv("../tgve/ksi-pf.csv")

# current data
ksi_lad = read_csv("~/code/saferactive/tgve/ksi.csv")
names(ksi_lad)
# add Mean per capita for LAD
ksi_lad$mean_km_percap = format(round(bounds$mean_km_percap, 2), nsmall = 2)
ksi_lad = ksi_lad[, c(1,2,3,4,5,6,8,7)] # reorder to keep lad_code at end
colnames(ksi_lad)[7] = "Mean km cycled per capita (2010-2019) based on resident population"
names(ksi_lad)
readr::write_csv(ksi_lad, "~/code/saferactive/tgve/ksi.csv")
