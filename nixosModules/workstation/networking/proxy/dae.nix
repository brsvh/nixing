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
  options.workstation.networking.proxy = {
    dae = mkOption {
      type = types.package;
      default = "dae";
      description = ''
        The package of dae.
      '';
    };
  };

  config = mkIf (withClient && withDae)
    {
      services = {
        dae = {
          enable = true;
          configFile = "${cfg.client.config}";
          package = cfg.dae;
        };
      };
    };
}
