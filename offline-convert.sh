#!/bin/bash

file=$1
converted_file="${file%.tex}.html"

echo "Converting $file to html"
echo "file: $file"
echo "converted_file: $converted_file"

pandoc --standalone "$file" -o "$converted_file" --mathjax --citeproc --toc
emacs --batch "$converted_file" -l ./tex2html.el --eval "(progn (tex2html-add-style-to-html-head) (tex2html-postprocess-html-buffer) (tex2html-add-comment-script) (tex2html-add-tex-pdf-links))" -f save-buffer
