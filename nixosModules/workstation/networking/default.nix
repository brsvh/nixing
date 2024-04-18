{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.workstation.networking;
in
{
  imports = [
    ./proxy

    ./bluetooth.nix
    ./firewall.nix
    ./network-manager.nix
    ./openssh.nix
  ];

  options.workstation.networking = {
    manager = mkOption {
      type = types.enum [ "network-manager" ];
      default = "network-manager";
      description = ''
        The networking manage backend.
      '';
    };
  };

  config = mkMerge [
    {
      services = {
        resolved = {
          enable = true;
        };
      };
    }
  ];
}
