#!/bin/bash  

quarto publish quarto-pub uwash.qmd --no-browser
rm -r *_files
rm *.html
