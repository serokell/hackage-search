#!/bin/sh

echo "Installing hackage-search"

nix-build . -A release
nix-env --set ./result --profile "/nix/var/nix/profiles/per-user/$USER/hackage-search"
