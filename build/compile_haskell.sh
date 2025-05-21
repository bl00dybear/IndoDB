#!/bin/bash
set -e
rm -f /home/cristi/projects/IndoDB/src/parser/*.o
rm -f /home/cristi/projects/IndoDB/src/parser/*.hi
cd /home/cristi/projects/IndoDB/src/parser
/home/cristi/.ghcup/bin/cabal v2-build
EXEC_PATH=$(find dist-newstyle -name sql_parser -type f -executable | head -n 1)
if [ -n "$EXEC_PATH" ]; then
  cp "$EXEC_PATH" /home/cristi/projects/IndoDB/output/
  echo "Compilare finalizată cu succes și executabilul a fost copiat în output/"
else
  echo "Eroare: Executabilul sql_parser nu a fost găsit în dist-newstyle/"
  exit 1
fi
