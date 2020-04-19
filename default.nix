let

  nixpkgsPin = import ./nix/nixpkgs-pin.nix;

in

{ pkgs ? import (builtins.fetchTarball nixpkgsPin) {},
  hc ? "ghc882"
}:

let

  haskell_inputs = p: [
    p.servant-server
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
    mkdir -p "$out/backend/" "$out/frontend/"

    mkdir backend-build-artifacts
    ghc "$src/backend/Main.hs" "$src/backend/Config.hs" \
      -outputdir backend-build-artifacts \
      -o "$out/backend/hackage-search"

    runhaskell "$src/frontend/Build.hs" --src "$src/frontend" --out "$out/frontend"
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
