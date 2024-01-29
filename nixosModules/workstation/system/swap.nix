{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.workstation.system;
in
{
  options.workstation.system = {
    swap = {
      enable = mkOption {
        type = types.bool;
        default = true;
        example = "true";
        description = ''
          Whether to enable SWAP support.
        '';
      };

      devices = mkOption {
        default = [ ];
        example =
          [
            { device = "/dev/hda7"; }
            { device = "/var/swapfile"; }
            { label = "bigswap"; }
          ];
        description = ''
          The swap devices and swap files.

          See `config.swapDevices` for more information.
        '';
        type = types.listOf types.unspecified;
      };
    };

    zram = {
      enable = mkOption {
        type = types.bool;
        default = true;
        example = "true";
        description = ''
          Whether to enable zram support.
        '';
      };

      percent = mkOption {
        default = 100;
        type = types.int;
        description = ''
          Maximum total amount of memory that can be stored in the
          zram swap devices.
        '';
      };

      priority = mkOption {
        default = 5;
        type = types.int;
        description = ''
          Priority of the zram swap devices.
        '';
      };

      algorithm = mkOption {
        default = "zstd";
        example = "lz4";
        type = with types; either (enum [ "lzo" "lz4" "zstd" ]) str;
        description = ''
          Compression algorithm.
        '';
      };
    };
  };

  config = mkMerge
    [
      (
        mkIf cfg.swap.enable
          {
            swapDevices = cfg.swap.devices;
          }
      )
      (
        mkIf cfg.zram.enable
          {
            zramSwap = {
              algorithm = cfg.zram.algorithm;
              enable = cfg.zram.enable;
              memoryPercent = cfg.zram.percent;
              priority = cfg.zram.priority;
            };
          }
      )
    ];
}
