#!/bin/bash
file=$1
converted_file="${file%.tex}.html"
pandoc --standalone "$file" -o "$converted_file" --mathjax --citeproc
