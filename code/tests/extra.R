
library(tidyverse)

dd = stats19:::get_data_directory()
dde = file.path(dd, "DfTRoadSafety")
list.files(dde)

# mm = vroom::vroom(file.path(dde, "2010_2019_Make_Model_1957306.csv"))
mm = readr::read_csv(file.path(dde, "2010_2019_Make_Model_1957306.csv"))
mm2 = vroom::vroom("~/stats19-data/dft-make-model/2015_Make_Model.csv")

skimr::skim(mm)

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
