#!/bin/bash

echo "Compiling $1"
latexmk -pdf -shell-escape -view=none -f -gg "$1"
