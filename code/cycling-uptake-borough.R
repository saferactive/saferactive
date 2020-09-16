library(tidyverse)
cycling_borough_year = readRDS(url("https://github.com/saferactive/saferactive/raw/master/predictions-borough-year.Rds"))
names(cycling_borough_year)
head(cycling_borough_year)
ggplot(cycling_borough_year) +
  geom_line(aes(year, borough_mean_cycles, colour = Name))

ggplot(cycling_borough_year) +
  geom_line(aes(year, borough_mean_cycles, colour = Name))

cycling_borough = cycling_borough_year %>%
  sf::st_drop_geometry() %>%
  group_by(Name) %>%
  mutate(
    `Selected boroughs` = case_when(
    Name == "Waltham Forest" ~ Name,
    mean(borough_mean_cycles) < 30 ~ NA_character_,
    TRUE ~ Name
    ),
    Cycling_2011 = case_when(
      year == 2011 ~ borough_mean_cycles
    ),
    Multiplier = borough_mean_cycles / mean(Cycling_2011, na.rm = TRUE)
  )
names_selected = unique(cycling_borough$`Selected boroughs`)
names_selected = c(names_selected[2:length(names_selected)], "Other")

ggplot(cycling_borough) +
  geom_line(aes(year, borough_mean_cycles, colour = `Selected boroughs`, group = Name), size = 1.2) +
  scale_color_discrete(labels = names_selected)

ggplot(cycling_borough) +
  geom_line(aes(year, Multiplier, colour = `Selected boroughs`, group = Name)) +
  scale_color_discrete(labels = names_selected)

ggplot(cycling_borough %>% filter(is.na(`Selected boroughs`))) +
  geom_line(aes(year, Multiplier, group = Name), colour = "grey", size = 0.2) +
  geom_line(
    aes(year, Multiplier, colour = `Selected boroughs`, group = Name),
    data = cycling_borough %>% filter(!is.na(`Selected boroughs`)),
    size = 1.2
    ) +
  scale_color_discrete(labels = names_selected)

write_csv(cycling_borough, "cycling_borough.csv")
piggyback::pb_upload("cycling_borough.csv")
