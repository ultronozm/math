#!/bin/bash

file=$1
converted_file="${file%.tex}.html"
emacs --batch "$converted_file" -l ./tex2html.el --eval "(progn (tex2html-add-style-to-html-head) (tex2html-postprocess-html-buffer) (tex2html-add-comment-script) (tex2html-add-tex-pdf-links))" -f save-buffer
