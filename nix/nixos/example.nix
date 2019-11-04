{ config, ... }:

let
  sources = import ../../nix/sources.nix;
  iohkLib = import ../../lib.nix { };
  cluster = "mainnet";
  targetEnv = iohkLib.cardanoLib.environments.${cluster};
  host = "explorer.example.com";
in {
  imports = [
    (sources.cardano-node + "/nix/nixos")
    ./cardano-exporter-service.nix
  ];
  services.cardano-node = {
    environment = cluster;
    topology = iohkLib.cardanoLib.mkEdgeTopology { edgeHost = iohkLib.cardanoLib.environments.${cluster}.edgeHost; edgePort = 7777; };
    enable = true;
  };
  services.cardano-exporter = {
    enable = true;
    inherit (targetEnv) genesisFile genesisHash;
    inherit cluster;
    socketPath = "/run/cardano-node/node-core-0.socket";
  };
  services.cardano-explorer-api.enable = true;
  services.nginx = {
    virtualHosts.${host} = {
      default = true;
      locations."/api/".extraConfig = ''
        proxy_pass http://localhost:8100/api/;
        proxy_set_header Host $host;
        proxy_set_header REMOTE_ADDR $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
      '';
    };
  };
  systemd.services = {
    cardano-explorer-node = {
      wants = [ "cardano-node.service" ];
      serviceConfig.PermissionsStartOnly = "true";
      preStart = ''
        chgrp cexplorer ${config.services.cardano-exporter.socketPath}
        chmod g+w ${config.services.cardano-exporter.socketPath}
      '';
    };
  };
}