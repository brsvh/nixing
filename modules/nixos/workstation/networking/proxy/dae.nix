{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.workstation.networking.proxy;

  withClient = cfg.client.enable;

  withDae = withClient && cfg.client.flavour == "dae";
in
{
  config = mkIf (withClient && withDae)
    {
      services = {
        dae = {
          enable = true;
          config = "${cfg.client.config}";
        };
      };
    };
}
