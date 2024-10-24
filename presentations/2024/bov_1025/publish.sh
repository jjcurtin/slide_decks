#!/bin/bash  

cp slides.css bov.css 
  quarto publish quarto-pub bov.qmd --no-browser
 
rm bov.css

# these commented out because needed for multiplex
# instead added to gitignore
# rm -r *_files
# rm *.html
