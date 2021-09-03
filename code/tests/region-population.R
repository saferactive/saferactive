
# Lookup from Local Authority District to Region (LADCD21)
# from https://geoportal.statistics.gov.uk/datasets/ons::local-authority-district-to-region-april-2021-lookup-in-england/about
lad_region_lookup = read_csv("Local_Authority_District_to_Region_(April_2021)_Lookup_in_England.csv")

# Local Authority District populations (LADCD20)
# from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
piggyback::pb_download("MYEB1_detailed_population_estimates_series_UK_.2019_geog20.csv", tag = "0.1.4")
pop_lad = read_csv("MYEB1_detailed_population_estimates_series_UK_.2019_geog20.csv")

# Convert 2020 codes to 2021 codes
code20 = unique(pop_lad$ladcode20)
code21 = unique(lad_region_lookup$LAD21CD)
unused = code20[! code20 %in% code21]

eng_former_lads = pop_lad %>%
  filter(
    ladcode20 %in% unused,
    grepl("E", ladcode20)
  )
codes_to_update = unique(eng_former_lads$ladcode20)

# update all Northamptonshire districts to new North Northamptonshire code (simply to allow the region lookup)
pop_lad$ladcode20[which(pop_lad$ladcode20 %in% codes_to_update)] = "E06000061"

# Convert from LAD to region
pop_region = inner_join(pop_lad, lad_region_lookup, by = c("ladcode20" = "LAD21CD"))

join_region = pop_region %>%
  group_by(RGN21CD, RGN21NM) %>%
  summarise(
    population_2001 = sum(population_2001),
    population_2002 = sum(population_2002),
    population_2003 = sum(population_2003),
    population_2004 = sum(population_2004),
    population_2005 = sum(population_2005),
    population_2006 = sum(population_2006),
    population_2007 = sum(population_2007),
    population_2008 = sum(population_2008),
    population_2009 = sum(population_2009),
    population_2010 = sum(population_2010),
    population_2011 = sum(population_2011),
    population_2012 = sum(population_2012),
    population_2013 = sum(population_2013),
    population_2014 = sum(population_2014),
    population_2015 = sum(population_2015),
    population_2016 = sum(population_2016),
    population_2017 = sum(population_2017),
    population_2018 = sum(population_2018),
    population_2019 = sum(population_2019),
    )

saveRDS(join_region, "region-populations.Rds")
