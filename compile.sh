#!/bin/bash
latexmk -pdf -shell-escape -view=none -f -gg "$1"
