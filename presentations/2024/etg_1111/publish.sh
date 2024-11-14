#!/bin/bash  

quarto publish quarto-pub etg.qmd --no-browser
rm -r *_files
rm *.html
