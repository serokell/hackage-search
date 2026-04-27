{
  description = "Hackage search";

  nixConfig.flake-registry = "https://github.com/serokell/flake-registry/raw/master/flake-registry.json";
  inputs = {
    serokell-nix.inputs.nixpkgs.follows = "nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";

    deploy-rs.url = "github:serokell/deploy-rs";
    deploy-rs.inputs.nixpkgs.follows = "nixpkgs";

    servant-prometheus = {
      url = "github:serokell/servant-prometheus";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, serokell-nix, flake-utils, deploy-rs, servant-prometheus, ... }:
    let
      inherit (nixpkgs.lib) recursiveUpdate makeLibraryPath;
      inherit (builtins) mapAttrs;
    in
    recursiveUpdate (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.extend serokell-nix.overlay;
        p = import ./package.nix { inherit pkgs; servant-prometheus = compiler: import servant-prometheus { nixpkgs = pkgs; inherit compiler; }; };
      in {
        defaultPackage = self.packages.${system}.hackage-search;
        packages.hackage-search = pkgs.buildEnv {
          name = "hackage-search-release";
          paths = [
            p.frontend
            p.download
            p.search
          ];
        };

        # Pass-through for the deployment tool
        packages.deploy-rs = deploy-rs.packages.${system}.deploy-rs;

        devShell = pkgs.mkShell rec {
          buildInputs = p.shellExtraInputs;

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
      module = import ./service.nix { inherit serokell-nix; };

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
