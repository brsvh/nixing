{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.workstation.networking.proxy;

  withClient = cfg.client.enable;

  withClash = withClient && cfg.client.flavour == "clash";
  withSingbox = withClient && cfg.client.flavour == "singbox";
  withV2ray = withClient && cfg.client.flavour == "v2ray";
  withV2rayA = withClient && cfg.client.flavour == "v2raya";
in
{
  imports = [
    ./dae.nix
    ./singbox.nix
    ./v2ray.nix
  ];

  options.workstation.networking.proxy = {
    client = {
      config = mkOption {
        type = with types; (nullOr path);
        default = null;
        description = ''
          The configuration of proxy client.
        '';
      };

      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Use proxy client or not.
        '';
      };

      flavour = mkOption {
        type = types.enum [
          "dae"
          "singbox"
          "v2ray"
        ];
        default = "dae";
        description = ''
          The flavour of proxy client.
        '';
      };

      port = mkOption {
        type = types.str;
        default = "1080";
        description = ''
          The port of proxy client.
        '';
      };
    };
  };
}
