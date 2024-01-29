{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.workstation.networking.proxy;

  withClient = cfg.client.enable;

  withV2ray = withClient && cfg.client.flavour == "v2ray";
in
{
  config = mkMerge
    [
      (
        mkIf (withClient && withV2ray)
          {
            services = {
              v2ray = {
                enable = true;
                configFile = "${cfg.client.config}";
              };
            };
          }
      )
      (
        mkIf withV2ray
          {
            networking = {
              proxy = {
                default = "socks5h://127.0.0.1:${cfg.client.port}";
              };
            };
          }
      )
    ];
}
