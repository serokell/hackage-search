{ pkgs
, servant-prometheus
, hc ? "ghc884"
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
      p.servant-blaze
      p.http-client-tls
      p.prometheus-client
      p.prometheus-metrics-ghc
      p.split
      p.tar
      p.unix
      p.uuid
      p.unagi-chan
      servant-prometheus
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
    pkgs.haskellPackages.html-validator-cli
    pkgs.ghcid
  ] ++
  (if pkgs.stdenv.isLinux then [pkgs.inotify-tools] else []);

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
        mkdir -p "$out/bin"
        mkdir backend-build-artifacts
        ghc "$src/Download.hs" \
          -outputdir backend-build-artifacts \
          -o "$out/bin/hackage-download" \
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
        mkdir -p "$out/bin"
        mkdir backend-build-artifacts
        ghc "$src/Search.hs" \
          -outputdir backend-build-artifacts \
          -o "$out/bin/hackage-search" \
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
        mkdir -p "$out/html"
        runghc "$src/Build.hs" \
          --src "$src" \
          --out "$out/html/index.html"
        cp "$src/favicon.svg" \
           "$out/html/favicon.svg"
      '';
      buildInputs = frontendInputs;
      inherit LOCALE_ARCHIVE;
    };

  shellExtraInputs = backendInputs ++ frontendInputs ++ shellInputs;
}
