let

  nixpkgsPin = import ./nix/nixpkgs-pin.nix;

in

{ pkgs ? import (builtins.fetchTarball nixpkgsPin) {},
  hc ? "ghc882"
}:

let

  haskell_inputs = p: [
    p.servant-server
    p.http-client-tls
    p.split
    p.tar
  ];

  haskellPackages =
    pkgs.haskell.packages.${hc}.override {
      overrides = self: super: rec {
        /* No overrides needed for now */
      };
    };

in

pkgs.stdenv.mkDerivation rec {
  name = "hackage-search";
  src = ./.;
  buildCommand = ''
    mkdir -p "$out"

    mkdir backend-build-artifacts
    ghc "$src/backend/Search.hs" \
      -outputdir backend-build-artifacts \
      -o "$out/hackage-search" \
      -Wall -threaded -O2 -with-rtsopts="-N"

    ghc "$src/backend/Download.hs" \
      -outputdir backend-build-artifacts \
      -o "$out/hackage-download" \
      -Wall -threaded -O2 -with-rtsopts="-N"

    runhaskell "$src/frontend/Build.hs" \
      --src "$src/frontend" \
      --out "$out/index.html"
  '';
  buildInputs = [
    /* Backend */
    (haskellPackages.ghcWithPackages haskell_inputs)
    pkgs.cabal-install
    pkgs.git
    pkgs.zlib
    pkgs.pkgconfig

    /* Frontend */
    pkgs.nodePackages.typescript
    pkgs.closurecompiler
    pkgs.sass

    /* Development */
    pkgs.inotify-tools
    pkgs.haskellPackages.html-validator-cli
    pkgs.ghcid
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH
    export LANG=en_US.UTF-8
  '';
  LOCALE_ARCHIVE =
    if pkgs.stdenv.isLinux
    then "${pkgs.glibcLocales}/lib/locale/locale-archive"
    else "";
}
