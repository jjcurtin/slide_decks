#!/bin/bash  

cp slides.css bov.css 
  quarto publish quarto-pub bov.qmd --no-browser
 
rm bov.css
rm -r *_files
rm *.html
