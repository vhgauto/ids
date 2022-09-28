# browseURL("https://www.youtube.com/watch?v=LKprlFCLnSA")

library(tidyverse)
library(archive)

setwd("\\\\wsl.localhost/Ubuntu/home/victor/ids")

# creo un 'archive' con los archivos .dly
archive_write_files(archive = "write_files.tar.gz",
                    files = c("data/ghcnd_all/AGM00060670.dly",
                              "data/ghcnd_all/ARM00087896.dly",
                              "data/ghcnd_all/ASN00001023.dly"))

# contiene 3 archivos .dly
archive(file = "write_files.tar.gz")

# contiene TODOS los archivos (~ 122000)
# A tibble: 122,044 × 3
archive(file = "data/ghcnd_all.tar.gz")

# leo el contenido del archivo 'tar.gz', el 1er elemento
archive_read(archive = "write_files.tar.gz",
             file = 1) %>% 
  read_tsv()
# puedo usar el nombre del archivo comprimido
archive_read(archive = "write_files.tar.gz",
             file = "data/ghcnd_all/AGM00060670.dly") %>% 
  read_csv()

# generalizo para todos los archivos dentro de 'write_files.tar.gz'
archive(file = "write_files.tar.gz") %>% 
  # extraigo 'path' como vector
  pull(path) %>% 
  # leo el contenido de c/archivo y creo un único tibble
  map_dfr(., ~ read_tsv(file = archive_read(archive = "write_files.tar.gz",
                                            file = .x)))

