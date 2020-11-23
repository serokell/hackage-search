{
  description = "Hackage search";

  inputs = {
    serokell-nix.url = "github:serokell/serokell.nix";
    nixpkgs.follows = "serokell-nix/nixpkgs";
    serokell-website.url = "git+ssh://git@github.com/serokell/serokell-website";

    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    deploy-rs.url = "github:serokell/deploy-rs";
    deploy-rs.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, serokell-nix, flake-utils, deploy-rs, ... }@inputs:
    let
      inherit (nixpkgs.lib) recursiveUpdate makeLibraryPath;
      inherit (builtins) mapAttrs;
    in
    recursiveUpdate (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.extend serokell-nix.overlay;
        p = import ./package.nix { inherit pkgs; };
      in {
        defaultPackage = self.packages.${system}.hackage-search;
        packages.hackage-search = pkgs.buildEnv {
          name = "hackage-search-release";
          paths = [
            p.frontend
            p.download
            p.search
            inputs.serokell-website.packages.x86_64-linux.fonts
          ];
        };

        devShell = pkgs.mkShell rec {
          buildInputs = [
            pkgs.nixUnstable
            deploy-rs.defaultPackage.${system}
          ] ++ p.shellExtraInputs;

          shellHook = ''
            export LD_LIBRARY_PATH=${makeLibraryPath buildInputs}:$LD_LIBRARY_PATH
            export LANG=en_US.UTF-8
          '';

          LOCALE_ARCHIVE =
            if pkgs.stdenv.isLinux
            then "${pkgs.glibcLocales}/lib/locale/locale-archive"
            else "";
        };
      }))

    {
      module = import ./service.nix;

      # Deployment expressions
      deploy.magicRollback = false;
      deploy.nodes.sadalbari = let
        system = "x86_64-linux";
      in {
        hostname = "sadalbari.pegasus.serokell.team";
        sshOpts = [ "-p" "17788" ];
        profiles.hackage-search = {
          sshUser = "deploy";
          path = deploy-rs.lib.${system}.activate.custom
            self.packages.${system}.hackage-search
              "sudo systemctl restart hackage-search";
        };
      };

      checks = mapAttrs (_: lib: lib.deployChecks self.deploy) deploy-rs.lib;
    };
}
