#!/bin/bash

emacs --batch -l ./tex2html.el --eval "(tex2html-make-index)"
