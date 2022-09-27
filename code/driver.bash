#!/usr/bin/env bash


# obtengo los datos diarios de todas las estaciones climáticas y
# genero una lista con las estaciones
code/get_ghcnd_data.bash ghcnd_all.tar.gz
code/get_ghcnd_all_files.bash

# obtengo una lista de los tipos de datos para cada estación climática
code/get_ghcnd_data.bash ghcnd-inventory.txt

# obtengo los metadatos para cada estación climática
code/get_ghcnd_data.bash ghcnd-stations.txt

