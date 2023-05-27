#!/bin/bash

temp_file=$(mktemp)

cat > "$temp_file"

echo $(pwd)

# # Run Emacs in batch mode, executing the `test` function on stdin
emacs --batch -l test.el $temp_file --eval "(test)" -f save-buffer

cat "$temp_file"

rm "$temp_file"
