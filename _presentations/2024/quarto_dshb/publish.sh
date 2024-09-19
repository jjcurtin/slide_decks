#!/bin/bash  

quarto publish quarto-pub quarto.qmd --no-browser
rm -r *_files
rm *.html
