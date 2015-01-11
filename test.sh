#!/bin/bash

runtest () {
  if cabal build; then
    cabal test &>/dev/null
    cat dist/test/*-hspec.log
  fi
}

runtest

while true; do
  file=$(inotifywait -r -e close_write src test 2>/dev/null)
  file=${file#./*}
  if [[ "$file" == *.hs ]]; then
    runtest
  fi
done
