#!/bin/bash

if [ -d "$HOME/.cabal/bin" ] ; then
    PATH="$HOME/.cabal/bin:$PATH"
fi

pandoc -s chapters/*.md --highlight-style=etc/highlight.theme --from markdown+auto_identifiers -V colorlinks --number-sections --pdf-engine=xelatex --template=etc/template.latex --variable mainfont="Linux Libertine O" --dpi=300 --bibliography all.bib --filter render.hs -o pihkal.pdf
