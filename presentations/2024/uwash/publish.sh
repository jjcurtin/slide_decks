#!/bin/bash  

# ./render.sh FORMAT
# FORMAT = book, slides, or slides_wide

FORMAT=$1
 
if [ "$FORMAT" = "slides" ];  then
  echo ""
	echo "Publishing uwash.qmd to standard slides on quarto-pub"
  echo ""
  cp slides.css uwash.css 
  quarto publish quarto-pub uwash.qmd --no-browser
  rm -r *_files
  rm *.html
  git restore uwash.css 
fi
 
if [ "$FORMAT" = "slides_wide" ];  then
  echo ""
	echo "Publishing uwash.qmd to wide slides on quarto-pub"
  echo ""
  cp slides_wide.css uwash.css 
  quarto publish quarto-pub uwash.qmd --no-browser
  rm -r *_files
  rm *.html
  git restore uwash.css 
fi
