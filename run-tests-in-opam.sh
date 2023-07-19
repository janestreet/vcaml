#!/usr/bin/env bash
#
# Run the VCaml tests if the version of Neovim in PATH matches the version of Neovim
# against which this version of VCaml was tested. If we ran the tests without this check
# there may be failures that would be hard to understand.

set -euo pipefail
{
  EXPECTED_NVIM_VERSION="0.9.1"
  ACTUAL_NVIM_VERSION="$(nvim --version | head -n 1 | sed 's/^NVIM v//')"
  NAME="${1:-vcaml}"
  JOBS="${2:-${OPAMJOBS:-}}"

  if [[ "$EXPECTED_NVIM_VERSION" != "$ACTUAL_NVIM_VERSION" ]]; then
    cat >&2 << HERE
Found Neovim version $ACTUAL_NVIM_VERSION but this version of VCaml is built for $EXPECTED_NVIM_VERSION.
The VCaml tests might not work properly when a different version is used.
Please update the version of Neovim before attempting to run the tests.
HERE
    exit 1
  fi

  export OCAMLRUNPARAM="b=1"

  if [[ -z "$JOBS" ]]; then
    dune runtest -p "$NAME"
  else
    dune runtest -p "$NAME" -j "$JOBS"
  fi
}
