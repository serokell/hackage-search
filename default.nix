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
    mkdir -p $out/bin $out/home
    cp -r $src/backend .
    HOME=$out/home \
      cabal --offline --config-file /dev/null v2-install \
        --builddir=$out/dist \
        --installdir=$out/bin \
        exe:hackage-search
  '';
  buildInputs = [
    (haskellPackages.ghcWithPackages haskell_inputs)
    pkgs.cabal-install
    pkgs.git
    pkgs.zlib
    pkgs.pkgconfig
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
