#!/bin/bash

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

file=$1
converted_file="${file%.tex}.html"

echo "Converting $file to html"
echo "file: $file"
echo "converted_file: $converted_file"

if command -v pandoc &> /dev/null
then
    pandoc --standalone "$file" -o "$converted_file" --mathjax=https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js --citeproc --toc --wrap=none
elif command -v docker &> /dev/null
then
    docker run --volume "`pwd`:`pwd`" --workdir "`pwd`" pandoc/core:3.1 --standalone "$file" -o "$converted_file" --mathjax --citeproc --toc --wrap=none
    sudo chown runner:docker "$converted_file"
else
    echo "Neither pandoc nor docker found" >&2
    exit 1
fi

emacs --batch "$converted_file" -l "$script_dir/tex2html.el" --eval "(progn (tex2html-add-style-to-html-head) (tex2html-postprocess-html-buffer) (tex2html-add-comment-script) (tex2html-add-tex-pdf-links))" -f save-buffer

# Optional: a notes with sketches also gets a reader, in FILE-reader/.  Set SKETCHES_PYTHON to
# the python of an environment with sketches_html installed (https://github.com/ultronozm/sketches).
if [ -n "$SKETCHES_PYTHON" ] && grep -q '\\begin{sketch' "$file"; then
    "$SKETCHES_PYTHON" -m sketches_html --engine pandoc --aux "${file%.tex}.aux" --output "${file%.tex}-reader" "$file"
fi
