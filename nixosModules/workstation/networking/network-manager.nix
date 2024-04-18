{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.workstation.networking;

  withNetworkManager = cfg.manager == "network-manager";
in
{
  config = mkMerge [
    (mkIf withNetworkManager {
      networking = {
        networkmanager = {
          enable = true;
        };
      };
    })
  ];
}
