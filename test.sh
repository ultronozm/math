#!/bin/bash

temp_file=$(mktemp)

cat > "$temp_file"

# Run Emacs in batch mode, executing the `test` function on stdin
emacs --batch "$temp_file" -l ./test.el --eval "(test)" -f save-buffer

cat "$temp_file"

rm "$temp_file"
