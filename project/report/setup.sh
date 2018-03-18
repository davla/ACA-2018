#!/usr/bin/env bash

sudo apt-get install xdotool texlive latexmk
apm install pdf-view language-latex latex

echo -n '"pdf-view":
  fitToWidthOnOpen: true
latex:
  buildOnSave: true
  opener: "pdf-view"
  outputDirectory: "build"
' >> $HOME/.atom/config.cson
