browseURL("https://www.youtube.com/watch?v=nNKwcIfcwgo")

library(tidyverse)
library(glue)
library(lubridate)
library(archive)

setwd("\\\\wsl.localhost/Ubuntu/home/victor/ids")

# leo los archivos .dly
read_tsv("data/ghcnd_all/AGM00060670.dly") # NO funciona
read_table("data/ghcnd_all/AGM00060670.dly") # NO funciona

# reviso el README
browseURL("https://www.ncei.noaa.gov/pub/data/ghcn/daily/readme.txt")

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

# de esa tabla puedo extraer los anchos de columna para leer los .dly
# ancho de columna ID:      11 - 1 + 1 = 11
# ancho de columna YEAR:    15 - 12 + 1 = 4
# ancho de columna MONTH:   17 - 16 + 1 = 2
# ancho de columna ELEMENT: 21 - 18 + 1 = 4
# ...
# luego son cuartetos de: 5 espacios (VALUE), 1 espacio (MFLAG),
# 1 espacio (QFLAG), 1 espacio (SFLAG)
# que se repiten 31 veces en el mes

# los .dly tienen los datos almacenados en columnas de ancho fijo
# fwf: fixed width file

ancho <- c(11, 4, 2, 4, rep(c(5, 1, 1, 1), 31))
sum(ancho) # = 269, que la posición de la última columna (SFLAG31)

# encabezados de las columnas
# genero una función que cree los nombres
cuarteto <- function(x) {
  c(glue("VALUE{x}"), glue("MFLAG{x}"), glue("QFLAG{x}"), glue("SFLAG{x}"))
}

encabezado <- c("ID", "YEAR", "MONTH", "ELEMENT",
                unlist(map(.x = as.character(1:31), .f = cuarteto)))
length(encabezado) # son 128 columnas

read_fwf("../data/ghcnd_all/AGM00060670.dly") # SÍ funciona, pero las columnas
# están mal organizadas
?read_fwf

# de acuerdo al README las celdas con número '-9999' son en realidad 'NA'
# defino que todas las columnas sean leídas como 'character'
# me interesan algunas columnas (ID, YEAR, MONTH, ELEMENT y todas las VALUE)
read_fwf(file = "data/ghcnd_all/AGM00060670.dly",
         col_positions = fwf_widths(widths = ancho,
                                    col_names = encabezado),
         na = c("NA", "-9999", ""),
         col_types = cols(.default = col_character()),
         col_select = c(ID, YEAR, MONTH, ELEMENT, starts_with("VALUE"))) 

# generalizo para todos los .dly en 'ghcnd_all.tar.gz'
# ~ 120000 archivos, ¡¡¡PRECAUCIÓN!!!
# 'read_fwf' va a 'bind_row' los archivos
dly_files <- archive(file = "data/ghcnd_all.tar.gz") %>% 
  # remuevo la fila que contiene el path de la carpeta en sí
  filter(str_detect(string = path, pattern = "dly")) %>% 
  # extraigo 'path' como vector
  pull(path)

# leo el contenido de c/archivo dentro de 'ghcnd_all.tar.gz' y lo combino
# en un gran tibble, por filas
read_fwf(file = "data/ghcnd_cat.gz",
         col_positions = fwf_widths(widths = ancho,
                                    col_names = encabezado),
         na = c("NA", "-9999", ""),
         col_types = cols(.default = col_character()),
         col_select = c(ID, YEAR, MONTH, ELEMENT, starts_with("VALUE")))






  # extraer el contenido de 120 archivos lleva ~ 20 segundos
  # cambio los nombres de las columnas (todo a minúscula)
  rename_with(.cols = everything(), .fn = tolower) %>% 
  # sólo me interesan los datos de precipitación fff
  filter(element == "PRCP") %>% 
  # quito la columna 'element'
  select(-element) %>% 
  # tabla larga
  pivot_longer(cols = starts_with("value"),
               names_to = "day",
               values_to = "prcp") %>% 
  # quito los NA, porque se crearon filas p/el 29/02, 30/02, 31/02
  drop_na() %>% 
  # modifico 'day' para que solo contenga números
  mutate(day = str_replace(string = day, pattern = "value", replacement = "")) %>% 
  # creo la columna 'date'
  mutate(date = ymd(glue("{year}-{month}-{day}"))) %>% 
  # convierto 'prcp' a número
  mutate(prcp = as.numeric(prcp)) %>% 
  # 'prcp' está en decenas de milímetros (de acuerdo al README), lo paso a cm
  mutate(prcp = prcp/100) %>% 
  # elijo las columnas 
  select(id, date, prcp) %>% 
  # guardo
  write_tsv("data/composite_dly.tsv")
