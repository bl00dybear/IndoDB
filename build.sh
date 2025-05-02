#!/bin/bash
set -e  # Oprește scriptul la prima eroare

# Compilează Haskell
ghc ./src/parser/Main.hs -o ./src/parser/sql_parser

# Compilează C
gcc ./src/main.c ./src/cJSON.c -o ./src/main.o

# Rulează programul CLI
./src/main.o
