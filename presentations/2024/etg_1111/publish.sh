#!/bin/bash  

# ./render.sh FORMAT
# FORMAT = slides, or slides_wide

FORMAT=$1
 
if [ "$FORMAT" = "slides" ];  then
  quarto publish quarto-pub etg.qmd --no-browser
fi
 
if [ "$FORMAT" = "slides_wide" ];  then
  cp slides_wide.css chm.css 
  quarto publish quarto-pub chm.qmd --no-browser
fi

# rm -r *_files
# rm *.html
