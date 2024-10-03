#!/bin/bash  

# ./render.sh FORMAT
# FORMAT = slides, or slides_wide

FORMAT=$1
 
if [ "$FORMAT" = "slides" ];  then
  cp slides.css bov.css 
  quarto publish quarto-pub bov.qmd --no-browser
fi
 
if [ "$FORMAT" = "slides_wide" ];  then
  cp slides_wide.css bov.css 
  quarto publish quarto-pub bov.qmd --no-browser
fi

rm bov.css
rm -r *_files
rm *.html
