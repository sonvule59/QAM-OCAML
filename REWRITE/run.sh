#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"

if ! command -v dune >/dev/null 2>&1; then
  echo "Install the toolchain first. On macOS: brew install opam && opam init && eval \"\$(opam env)\" && opam install dune menhir ounit2" >&2
  exit 1
fi

dune build
exec dune exec -- main -- "$@"
