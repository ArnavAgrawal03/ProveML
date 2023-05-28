#!/bin/bash

LIB="ProveML"
LIBE="Prover"

if [[ "$OSTYPE" == "darwin"* ]]; then
  # macOS
  open _build/default/_doc/_html/
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
  if [[ -n "$IS_WSL" || -n "$WSL_DISTRO_NAME" ]]; then
    # WSL
    DOCPATH=$(wslpath -w ./_build/default/_doc/_html/)
    explorer.exe ${DOCPATH} || true
    # Why `|| true`? For unknown reasons, explorer.exe returns error code 1 even
    # when it succeeds in opening the path in a window.
  else
    nautilus _build/default/_doc/_html/
  fi
fi
