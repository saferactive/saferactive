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
