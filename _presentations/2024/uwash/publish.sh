#!/bin/bash  

# ./render.sh FILE FORMAT
# FILE = all or filename
# FORMAT = book, slides, or slides_wide

FORMAT=$1
FILE=$2
 
if [ "$FORMAT" = "slides" ];  then
  echo ""
	echo "Publishing $FILE to standard slides on quarto-pub"
  echo ""
  cp slides.css uwash.css 
  quarto publish quarto-pub "$FILE" --no-browser
  rm -r *_files
  rm *.html
  git restore uwash.css 
fi
 
if [ "$FORMAT" = "slides_wide" ];  then
  echo ""
	echo "Publishing $FILE to wide slides on quarto-pub"
  echo ""
  cp slides_wide.css uwash.css 
  quarto publish quarto-pub "$FILE" --no-browser
  rm -r *_files
  rm *.html
  git restore uwash.css 
fi
