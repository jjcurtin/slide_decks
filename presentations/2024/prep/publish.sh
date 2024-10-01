#!/bin/bash  

# ./render.sh FORMAT
# FORMAT = book, slides, or slides_wide

FORMAT=$1
 
if [ "$FORMAT" = "slides" ];  then
  echo ""
	echo "Publishing prep.qmd to standard slides on quarto-pub"
  echo ""
  cp slides.css prep.css 
  quarto publish quarto-pub prep.qmd --no-browser
  rm -r *_files
  rm *.html
  git restore prep.css 
fi
 
if [ "$FORMAT" = "slides_wide" ];  then
  echo ""
	echo "Publishing prep.qmd to wide slides on quarto-pub"
  echo ""
  cp slides_wide.css prep.css 
  quarto publish quarto-pub prep.qmd --no-browser
  rm -r *_files
  rm *.html
  git restore prep.css 
fi
