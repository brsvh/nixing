{ config
, lib
, pkgs
, ...
}:
with builtins;
with lib;
let
  cfg = config.workstation.networking.proxy;

  withClient = cfg.client.enable;

  withSingbox = withClient && cfg.client.flavour == "singbox";
in
{
  options.workstation.networking.proxy = {
    singbox = mkOption {
      type = types.package;
      default = "sing-box";
      description = ''
        The package of sing-box.
      '';
    };
  };

  config = mkIf (withClient && withSingbox)
    {
      services = {
        sing-box = {
          enable = true;
          package = cfg.singbox;
          settings = fromJSON (readFile cfg.client.config);
        };
      };
    };
}
