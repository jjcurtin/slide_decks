#!/bin/bash  

quarto publish quarto-pub bov.qmd --no-browser

# these commented out because needed for multiplex
# instead added to gitignore
# rm -r *_files
# rm *.html
