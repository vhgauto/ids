#!/usr/bin/env Rscript

# browseURL("https://www.youtube.com/watch?v=3rVvgfcpM-k")

library(tidyverse)
library(glue)
library(lubridate)
library(showtext)

# fuentes
font_add_google(name = "Merriweather", family = "merriweather")
font_add_google(name = "Lato", family = "lato")
showtext_auto()
showtext_opts(dpi = 300)

prcp_data <- read_tsv(file = "data/ghcnd_tidy.tsv.gz")

station_data <- read_tsv(file = "data/ghcnd_regions_years.tsv")

buffered_end <- today() - 5
buffered_start <- buffered_end - 30

lat_long_prcp <- inner_join(prcp_data, station_data, by = "id") %>% 
  # remuevo años inicial y final, excepto 2022
  filter((year != first_year & year != last_year) | year == 2022) %>% 
  group_by(latitude, longitude, year) %>% 
  summarise(mean_prcp = mean(prcp), .groups = "drop")

inicio <- case_when(
  month(buffered_start) == month(buffered_end) &
  year(buffered_start) == year(buffered_end) ~ format(buffered_start, "%d"),
  month(buffered_start) != month(buffered_end) &
  year(buffered_start) == year(buffered_end) ~ format(buffered_start, "%d/%m"),
  month(buffered_start) != month(buffered_end) &
  year(buffered_start) != year(buffered_end) ~ format(buffered_start, "%d/%m de %Y"),
  TRUE ~ format(buffered_start, "%d/%m de %Y")
)

fin <- case_when(
  month(buffered_start) == month(buffered_end) &
  year(buffered_start) == year(buffered_end) ~ format(buffered_end, "%d/%m de %Y"),
  month(buffered_start) != month(buffered_end) &
  year(buffered_start) == year(buffered_end) ~ format(buffered_end, "%d/%m de %Y"),
  month(buffered_start) != month(buffered_end) &
  year(buffered_start) != year(buffered_end) ~ format(buffered_end, "%d/%m de %Y"),
  TRUE ~ format(buffered_end, "%d/%m de %Y")
)

rango_fecha <- glue("{inicio} al {fin}"); rango_fecha

gg_ids <- lat_long_prcp %>% 
  group_by(latitude, longitude) %>% 
  mutate(# z-score
         z_score = mean_prcp - mean(mean_prcp)/sd(mean_prcp),
         # cantidad de años, 'n'
         n = n()) %>% 
  ungroup() %>%
  # al menos 50 años de datos
  filter(n >= 50 & year == year(buffered_end)) %>%
  # elimino la columna de la cantidad de años
  select(latitude, longitude, z_score) %>%
  # como la dispersión de 'z_score' es alta, dejo fijo los valores extremos
  mutate(z_score = case_when(z_score > 2 ~ 2,
                             z_score < -2 ~ -2,
                             TRUE ~ z_score)) %>%
  ggplot(aes(x = longitude, y = latitude, fill = z_score)) +
  geom_tile() +
  # obtengo una paleta de colores de: 
  scale_fill_gradient2(low = "#d8b365", mid = "#f5f5f5", high = "#5ab4ac",
                      midpoint = 0, breaks = c(-2, -1, 0, 1, 2),
                      labels = c("<-2", "-1", "0", "+1", ">+2")) +
  labs(fill = NULL, x = NULL, y = NULL,
       title = glue("Cantidad de precipitación, del {rango_fecha}"),
       subtitle = "z-score estandarizados, de al menos los últimos 50 años",
       caption = "Datos de precipitación recolectados diariamente de GHCN por NOAA") +
  coord_fixed() +
  # theme_bw() +
  theme(
    plot.background = element_rect(fill = "black", color = "black"),
    plot.title = element_text(color = "#f5f5f5", size = 20,
                              family = "merriweather"),
    plot.subtitle = element_text(color = "#f5f5f5", size = 12,
                                 family = "lato"),
    plot.caption = element_text(color = "#f5f5f5", size = 9,
                                family = "lato"),
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    legend.background = element_blank(),
    legend.position = c(.1, 0.05),
    legend.direction = "horizontal",
    legend.key.height = unit(.5, "line"),
    legend.text = element_text(color = "#f5f5f5", size = 7,
                               family = "lato"),
    axis.text = element_blank()
  )

ggsave(filename = "visuals/world_ids.png",
      plot = gg_ids,
      height = 10,
      width = 20,
      units = "cm",
      dpi = 300)
