# LA Trends
library(sf)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

# Read in Data
crash <- readRDS("crashes_active.Rds")
crash <- st_as_sf(crash[!is.na(crash$latitude),], coords = c("longitude","latitude"), crs = 4326)

download.file("https://github.com/saferactive/saferactive/releases/download/0.1/lads.Rds",
              destfile = "lads.Rds", mode = "wb")

la <- readRDS("lads.Rds")
la <- la[,c("Name")]
names(la) <- c("local_authority","geometry")

crash <- st_transform(crash, 27700)
la <- st_transform(la, 27700)
# Join on the LAs

crash <- st_join(crash, la)

# Get Year and Month
crash$year <- year(crash$date)

crash_yr <- crash %>%
  st_drop_geometry() %>%
  group_by(local_authority, year) %>%
  summarise(total_crash = n(),
            crash_fatal = length(accident_severity == "Fatal"),
            crash_serious = length(accident_severity == "Serious"),
            crash_slight = length(accident_severity == "Slight")
            )

# Simple Plots
rank_2018 <- crash_yr[crash_yr$year == 2018,]
rank_2018 <- rank_2018[order(rank_2018$total_crash, decreasing = TRUE),]
top_10_2018 <- rank_2018$local_authority[1:10]
bottom_10_2018 <- rank_2018$local_authority[seq(nrow(rank_2018) - 9,nrow(rank_2018))]

rank_2009 <- crash_yr[crash_yr$year == 2018,]
rank_2009 <- rank_2009[order(rank_2009$total_crash, decreasing = TRUE),]
top_10_2009 <- rank_2009$local_authority[1:10]
bottom_10_2009 <- rank_2009$local_authority[seq(nrow(rank_2009) - 9,nrow(rank_2009))]

ggplot(crash_yr[crash_yr$local_authority %in% top_10_2018,],
       aes(year, total_crash, colour = local_authority)) +
  geom_line() +
  ylab("Crashes with active travel casualties") +
  labs(color = "Local Authority") +
  ggtitle("Top 10 LAs in 2018") +
  ggsave("figures/la_top10.png")

ggplot(crash_yr[crash_yr$local_authority %in% bottom_10_2018,],
       aes(year, total_crash, colour = local_authority)) +
  geom_line() +
  ylab("Crashes with active travel casualties") +
  labs(color = "Local Authority") +
  ggtitle("Bottom 10 LAs in 2018") +
  ggsave("figures/la_bottom10.png")

ggplot(crash_yr[crash_yr$local_authority %in% top_10_2009,],
       aes(year, total_crash, colour = local_authority)) +
  geom_line() +
  ylab("Crashes with active travel casualties") +
  labs(color = "Local Authority") +
  ggtitle("Top 10 LAs in 2009")

ggplot(crash_yr[crash_yr$local_authority %in% bottom_10_2009,],
       aes(year, total_crash, colour = local_authority)) +
  geom_line() +
  ylab("Crashes with active travel casualties") +
  labs(color = "Local Authority") +
  ggtitle("Bottom 10 LAs in 2009")

# Biggest Changes 2009 - 2018

crash_yr_wide <- pivot_wider(crash_yr[,c("local_authority","year","total_crash")],
                             values_from = "total_crash",
                             names_from = "year")

crash_yr_wide$change <- (crash_yr_wide$`2018` - crash_yr_wide$`2009`) / crash_yr_wide$`2009` * 100
summary(crash_yr_wide$change)
crash_yr_wide <- crash_yr_wide[order(crash_yr_wide$change, decreasing = TRUE),]

change_top_10 <- crash_yr_wide$local_authority[1:10]
change_bottom_10 <- crash_yr_wide$local_authority[seq(nrow(crash_yr_wide) - 9,nrow(crash_yr_wide))]


ggplot(crash_yr[crash_yr$local_authority %in% change_top_10,],
       aes(year, total_crash, colour = local_authority)) +
  geom_line() +
  ylab("Crashes with active travel casualties") +
  labs(color = "Local Authority") +
  ggtitle("10 LAs with greatest % increase in crashes") +
  ggsave("figures/la_top10_increase.png")

ggplot(crash_yr[crash_yr$local_authority %in% change_bottom_10,],
       aes(year, total_crash, colour = local_authority)) +
  geom_line() +
  ylab("Crashes with active travel casualties") +
  labs(color = "Local Authority") +
  ggtitle("10 LAs with greatest % decrease in crashes") +
  ggsave("figures/la_top10_decrease.png")
