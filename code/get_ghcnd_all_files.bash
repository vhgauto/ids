#!/usr/bin/env bash

echo "fila_name" > data/ghcnd_all_files.txt
tar tf data/ghcnd_all.tar.gz | grep ".dly" >> data/ghcnd_all_files.txt