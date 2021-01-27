
library(tidyverse)

dd = stats19:::get_data_directory()
dde = file.path(dd, "DfTRoadSafety")
# browseURL(dd)
(f = list.files(dde))

# mm = vroom::vroom(file.path(dde, "2010_2019_Make_Model_1957306.csv"))
mm = readr::read_csv(file.path(dde, f[2])) ##
# mm2 = vroom::vroom("~/stats19-data/dft-make-model/2015_Make_Model.csv")

skimr::skim(mm)
head(mm$model)
mm %>%
  filter(str_detect(string = model, pattern = "ROOMSTER")) %>%
  pull(model) %>%
  unique()

# make/model, n. cars, classification
mm %>%
  mutate(model = paste(make, model)) %>%
  pull(model) %>%
  unique()


mm_mod = mm %>% mutate(
  make_updated = fct_lump_n(make, n = 8),
  make_updated20 = fct_lump_n(make, n = 20),
  mod_updated = fct_lump(model, n = 10)
  )
table(mm_mod$make_updated)
table(mm_mod$make_updated20)
table(mm_mod$mod_updated)
ggplot(mm_mod) +
  geom_bar(aes(make_updated))

# bug with vroom
# ch = vroom::vroom(file.path(dde, "2010_2019_Cycle Helmets_167206.csv"))
ch = readr::read_csv(file.path(dde, "2010_2019_Cycle Helmets_167206.csv"))
skimr::skim(ch)
table(ch$c20lab)
table(ch$c16lab)


# vt ----------------------------------------------------------------------

u = "https://www.eea.europa.eu/data-and-maps/data/co2-cars-emission-18/co2-emissions-cars-2017-provisional/co2_passengers_cars_v15_csv/at_download/file"
download.file(u, "~/hd/data/stats19-private/co2.zip")
dir.create("~/hd/data/stats19-private/co2")
unzip("~/hd/data/stats19-private/co2.zip", exdir = "~/hd/data/stats19-private/co2")
(f = list.files("~/hd/data/stats19-private/co2", full.names = TRUE))
# every single car type
co2_ue = readr::read_tsv(f[2])
co2_ue = data.table::fread(f)

# https://www.eea.europa.eu/data-and-maps/data/co2-cars-emission-18/co2-emissions-cars-2019-provisional


vt = read_csv("~/onedrive/projects-all/saferactive/make-model-data/genmodel_classification.csv")
vt

# todo: adjust make and model, find most common
