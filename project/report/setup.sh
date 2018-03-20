#!/usr/bin/env bash

BUILD_DIR='build'

sudo apt-get install xdotool texlive latexmk python-pygments texlive-latex-extra
apm install pdf-view language-latex latex

mkdir -p "$BUILD_DIR"

echo -n "\"pdf-view\":
  fitToWidthOnOpen: true
latex:
  buildOnSave: true
  opener: \"pdf-view\"
  outputDirectory: '$BUILD_DIR'
" >> $HOME/.atom/config.cson
