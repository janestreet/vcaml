#!/usr/bin/env bash
#
# Initialize a VCaml project that tracks janestreet-bleeding. The initial contents of the
# project will be the example plugins. This script is meant to be used by early adopters
# who do not mind using the bleeding versions of Jane Street packages for VCaml projects.

set -euo pipefail
{
  if [[ "$#" -ne 1 ]]; then
    printf "Usage: %s [NAME]\nMust provide a project name.\n" "$0"
    exit 1
  fi
  if [[ -d "$1" ]]; then
    echo "Project $1 already exists."
    exit 1
  fi
  echo "Downloading examples..."
  git clone --depth 1 https://github.com/janestreet/vcaml "$1"
  cd "$1"
  tmp="$(mktemp -d)"
  mv dune-project "$tmp"
  rm plugin/example/README.md
  mv plugin/example/* "$tmp"
  cd ..
  rm -rf "$1"
  mv "$tmp" "$1"
  cd "$1"
  echo "Updating the opam repositories..."
  opam update -y
  echo "Initializing a local opam switch to track janestreet-bleeding..."
  opam init --bare -n
  opam repository add --dont-select janestreet-bleeding https://ocaml.janestreet.com/opam-repository
  opam repository add --dont-select janestreet-bleeding-external https://github.com/janestreet/opam-repository.git#external-packages
  opam switch create --repositories=janestreet-bleeding,janestreet-bleeding-external,default . 5.2.0
  eval "$(opam env)"
  echo "Installing dune, utop, and vcaml..."
  opam install dune utop vcaml -y
  cat << HERE
Done. Now run:
cd $1
eval "\$(opam env)"
dune runtest

Then to get started, take a look at the READMEs:
* https://github.com/janestreet/vcaml/blob/master/README.md
* https://github.com/janestreet/vcaml/blob/master/plugin/example/README.md
HERE
}
