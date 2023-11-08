{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.workstation.networking.proxy;

  withClient = cfg.client.enable;

  withClash = withClient && cfg.client.flavour == "clash";
  withV2ray = withClient && cfg.client.flavour == "v2ray";
  withV2rayA = withClient && cfg.client.flavour == "v2raya";
in
{
  options = {
    workstation = {
      networking = {
        proxy = {
          client = {
            enable = mkOption {
              type = types.bool;
              default = false;
              description = ''
                Use proxy client or not.
              '';
            };

            flavour = mkOption {
              type = types.enum
                [
                  "clash"
                  "dae"
                  "v2ray"
                  "v2raya"
                ];
              default = "clash";
              description = ''
                The flavour of proxy client.
              '';
            };
          };
        };
      };
    };
  };

  config = mkMerge [
    (
      mkIf withClash
        {
          networking = {
            proxy = {
              default = "socks5h://127.0.0.1:7891";
            };
          };
        }
    )
    (
      mkIf withV2ray
        {
          networking = {
            proxy = {
              default = "socks5h://127.0.0.1:1080";
            };
          };
        }
    )
    (
      mkIf withV2rayA
        {
          networking = {
            proxy = {
              default = "socks5h://127.0.0.1:20170";
            };
          };
        }
    )
  ];
}
