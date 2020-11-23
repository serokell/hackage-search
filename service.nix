# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: MPL-2.0

{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.services.hackage-search;
in {
  options.services.hackage-search = {
    enable = mkEnableOption "Hackage search server";

    port = mkOption {
      description = "A port for running hackage search server on";
      type = with types; nullOr port;
      default = null;
    };

    socket = mkOption {
      description = "Unix domain socket to serve the server on";
      type = with types; nullOr str;
      default = "/run/hackage-search/server.sock";
    };

    package = mkOption {
      description = "A hackage-search package";
      type = with types; str;
      # Updated from https://github.com/serokell/hackage-search/tree/release by buildkite pipeline
      default = "/nix/var/nix/profiles/per-user/deploy/hackage-search";
    };
  };
  config = mkIf (cfg.enable) {
    assertions = [
      { assertion = isNull cfg.socket != isNull cfg.port;
        message = "Only one of socket and port may be defined at once."; }
    ];

    systemd.services = {
      hackage-download = rec {
        requires = [ "network-online.target" ];
        after = requires;

        path = [ cfg.package ];

        startAt = "daily";
        script = ''hackage-download --hackage "$CACHE_DIRECTORY"'';

        serviceConfig = {
          DynamicUser = true;
          User = "hackage-search";

          CacheDirectory = "hackage-search";
          WorkingDirectory = "/var/cache/hackage-search";
          Type = "oneshot";
        };
      };

      hackage-search = rec {
        wantedBy = [ "multi-user.target" ];

        requires = [ "hackage-download.service" ];
        after = requires;

        path = with pkgs; [ ripgrep cfg.package ];

        script =
          let serve =
            if ! isNull cfg.socket
            then "--unix ${cfg.socket}"
            else "--port ${toString cfg.port}";
          in ''
            hackage-search ${serve} --frontend "${cfg.package}/html" --hackage "$CACHE_DIRECTORY"
          '';

        serviceConfig = {
          DynamicUser = true;
          User = "hackage-search";

          ExecStartPost =
            if isNull cfg.socket
            then null
            else pkgs.writeShellScript "chmod-socket"
              "sleep 5; chmod 777 ${cfg.socket}";

          CacheDirectory = "hackage-search";
          RuntimeDirectory =
            if isNull cfg.socket
            then null
            else "hackage-search";
        };
      };
    };

    services.nginx.virtualHosts.hackage-search = {
      locations."/".proxyPass =
        if ! isNull cfg.socket
        then "http://unix:${cfg.socket}:/"
        else "http://localhost:${toString cfg.port}/";

      locations."/static/fonts/".alias = "${cfg.package}/fonts/";
    };
  };
}
