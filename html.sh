#!/bin/bash

if [ -d "$HOME/.cabal/bin" ] ; then
    PATH="$HOME/.cabal/bin:$PATH"
fi

pandoc -s 0*.md --from markdown+auto_identifiers+inline_code_attributes -V colorlinks --number-sections --template=templates/default.html --bibliography all.bib --filter render.hs -o pihkal.html
