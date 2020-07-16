let

  nixpkgsPin = import ./nix/nixpkgs-pin.nix;

in { pkgs ? import (builtins.fetchTarball nixpkgsPin) { }, hc ? "ghc882" }:

let

  haskell_inputs = p: [ p.servant-server p.http-client-tls p.split p.tar ];

  haskellPackages = pkgs.haskell.packages.${hc}.override {
    overrides = self: super:
      rec {
        # No overrides needed for now
      };
  };

  ghc = haskellPackages.ghcWithPackages haskell_inputs;

  backendInputs = [
    ghc
    pkgs.cabal-install
    pkgs.git
    pkgs.zlib
    pkgs.pkgconfig
  ];
  LOCALE_ARCHIVE = if pkgs.stdenv.isLinux then
    "${pkgs.glibcLocales}/lib/locale/locale-archive"
  else
    "";

in rec {
  search = pkgs.stdenv.mkDerivation rec {
    name = "hackage-search";
    src = ./backend;
    buildCommand = ''
      mkdir -p "$out/bin"

      mkdir backend-build-artifacts
      ghc "$src/Search.hs" \
        -outputdir backend-build-artifacts \
        -o "$out/bin/hackage-search" \
        -Wall -threaded -O2 -with-rtsopts="-N"
    '';
    inherit LOCALE_ARCHIVE;
    buildInputs = backendInputs;
  };

  download = pkgs.stdenv.mkDerivation rec {
    name = "hackage-download";
    src = ./backend;
    buildCommand = ''
      mkdir -p "$out/bin"

      mkdir backend-build-artifacts
      ghc "$src/Download.hs" \
        -outputdir backend-build-artifacts \
        -o "$out/bin/hackage-download" \
        -Wall -threaded -O2 -with-rtsopts="-N"
    '';
    inherit LOCALE_ARCHIVE;
    buildInputs = backendInputs;

  };

  frontend = pkgs.stdenv.mkDerivation rec {
    name = "hackage-search-frontend";
    src = ./frontend;
    buildCommand = ''
      mkdir -p "$out/share/frontend"

      runhaskell "$src/Build.hs" \
        --src "$src" \
        --out "$out/share/frontend/index.html"
    '';
    buildInputs =
      [ ghc pkgs.nodePackages.typescript pkgs.closurecompiler pkgs.sass ];
  };

  shell = pkgs.mkShell rec {
    nobuildPhase = "touch $out";
    inputsFrom = [ search download frontend ];
    buildInputs =
      [ pkgs.inotify-tools pkgs.haskellPackages.html-validator-cli pkgs.ghcid ];
    shellHook = ''
      export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath (buildInputs ++ builtins.concatMap (pkg: pkg.buildInputs) inputsFrom)}:$LD_LIBRARY_PATH
      export LANG=en_US.UTF-8
    '';
    inherit LOCALE_ARCHIVE;
  };
}
