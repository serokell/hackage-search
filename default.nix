let

  nixpkgsPin = import ./nix/nixpkgs-pin.nix;

in

{ pkgs ? import (builtins.fetchTarball nixpkgsPin) {},
  hc ? "ghc882"
}:

let

  haskellPackages =
    pkgs.haskell.packages.${hc}.override {
      overrides = self: super: rec {
        /* No overrides needed for now */
      };
    };

  ghcWithPackages =
    haskellPackages.ghcWithPackages (p: [
      p.servant-server
      p.http-client-tls
      p.split
      p.tar
    ]);

  backendInputs = [
    ghcWithPackages
    pkgs.cabal-install
    pkgs.git
    pkgs.zlib
    pkgs.pkgconfig
  ];

  frontendInputs = [
    ghcWithPackages /* for the build script */
    pkgs.nodePackages.typescript
    pkgs.closurecompiler
    pkgs.sass
  ];

  shellInputs = [
    pkgs.inotify-tools
    pkgs.haskellPackages.html-validator-cli
    pkgs.ghcid
  ];

  LOCALE_ARCHIVE =
    if pkgs.stdenv.isLinux
    then "${pkgs.glibcLocales}/lib/locale/locale-archive"
    else "";
in

rec {

  download =
    pkgs.stdenv.mkDerivation rec {
      name = "hackage-download";
      src = ./backend;
      buildCommand = ''
        mkdir -p "$out"
        mkdir backend-build-artifacts
        ghc "$src/Download.hs" \
          -outputdir backend-build-artifacts \
          -o "$out/hackage-download" \
          -Wall -threaded -O2 -with-rtsopts="-N"
      '';
      buildInputs = backendInputs;
      inherit LOCALE_ARCHIVE;
    };

  search =
    pkgs.stdenv.mkDerivation rec {
      name = "hackage-search";
      src = ./backend;
      buildCommand = ''
        mkdir -p "$out"
        mkdir backend-build-artifacts
        ghc "$src/Search.hs" \
          -outputdir backend-build-artifacts \
          -o "$out/hackage-search" \
          -Wall -threaded -O2 -with-rtsopts="-N"
      '';
      buildInputs = backendInputs;
      inherit LOCALE_ARCHIVE;
    };

  frontend =
    pkgs.stdenv.mkDerivation rec {
      name = "hackage-search-frontend";
      src = ./frontend;
      buildCommand = ''
        mkdir -p "$out"
        runghc "$src/Build.hs" \
          --src "$src" \
          --out "$out/index.html"
      '';
      buildInputs = frontendInputs;
      inherit LOCALE_ARCHIVE;
    };

  shell =
    pkgs.mkShell rec {
      nobuildPhase = "touch $out";
      buildInputs = backendInputs ++ frontendInputs ++ shellInputs;
      shellHook = ''
        export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH
        export LANG=en_US.UTF-8
      '';
      inherit LOCALE_ARCHIVE;
    };

}
