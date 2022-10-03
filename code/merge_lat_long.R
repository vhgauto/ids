#!/usr/bin/env Rscript

# browseURL("https://www.youtube.com/watch?v=gy2jaP_OK_c")

library(tidyverse)

setwd("//wsl.localhost/Ubuntu/home/victor/ids")

# reviso el README
# browseURL("https://www.ncei.noaa.gov/pub/data/ghcn/daily/readme.txt")

# ------------------------------
#   Variable   Columns   Type
# ------------------------------
#   ID            1-11   Character
# YEAR         12-15   Integer
# MONTH        16-17   Integer
# ELEMENT      18-21   Character
# VALUE1       22-26   Integer
# MFLAG1       27-27   Character
# QFLAG1       28-28   Character
# SFLAG1       29-29   Character
# VALUE2       30-34   Integer
# MFLAG2       35-35   Character
# QFLAG2       36-36   Character
# SFLAG2       37-37   Character
# .           .          .
# .           .          .
# .           .          .
# VALUE31    262-266   Integer
# MFLAG31    267-267   Character
# QFLAG31    268-268   Character
# SFLAG31    269-269   Character
# ------------------------------


read_fwf("data/ghcnd-stations.txt",
         col_positions = fwf_cols(
           id = c(1, 11),
           latitude = c(13, 20),
           longitude = c(22, 30),
           elevation = c(32, 37),
           state = c(39, 40),
           name = c(42, 71),
           gsn_flag = c(73, 75),
           hcn_flag = c(77, 79),
           wmo_id = c(81, 85)),
         col_select = c(id, latitude, longitude)) %>% 
         mutate(latitude = round(latitude, 0),
                longitud = round(longitude, 0)) %>% 
  group_by(latitude, longitude) %>% 
  mutate(region = cur_group_id()) %>% 
  write_tsv("data/ghcnd_regions.tsv")







         