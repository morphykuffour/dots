#!/usr/bin/env bash
# Launch Emacs built with Nix flake
cd "$(dirname "$0")"
exec nix run .#default -- "$@"
