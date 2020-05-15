# Get pct commute data for London. Ensure journeys to/from outside London are included
library(pct)
library(stplanr)

l = pct::get_pct_lines(region = "london")

l = l %>%
  select(c(1:11)) %>%
  filter(bicycle > 0)

unique(l$geo_name2)

r = route(l, route_fun = cyclestreets::journey())

# Route journeys to work.


# Count length of journeys within each London Borough


# Count cycle casualties within each London Borough


# Calculate casualties per billion km
