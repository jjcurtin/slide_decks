#!/bin/bash  

FILENAME=$1

quarto publish quarto-pub "$FILENAME" --no-browser
rm -r *_files
rm *.html
