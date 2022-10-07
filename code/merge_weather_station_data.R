#!/usr/bin/env Rscript

# browseURL("https://www.youtube.com/watch?v=3rVvgfcpM-k")

library(tidyverse)

setwd("//wsl.localhost/Ubuntu/home/victor/ids")

prcp_data <- read_tsv(file = "data/ghcnd_tidy.tsv.gz")

station_data <- read_tsv(file = "data/ghcnd_regions_years.tsv")

# anti_join(prcp_data, station_data, by = "id")
# anti_join(station_data, prcp_data, by = "id")

lat_long_prcp <- inner_join(prcp_data, station_data, by = "id") %>% 
  # remuevo años inicial y final, excepto 2022
  filter((year != first_year & year != last_year) | year == 2022) %>% 
  group_by(latitude, longitude, year) %>% 
  summarise(mean_prcp = mean(prcp)) %>% 
  summarise(n = n())

lat_long_prcp %>% 
  ggplot(aes(x = n)) +
  geom_histogram() +
  theme_classic()