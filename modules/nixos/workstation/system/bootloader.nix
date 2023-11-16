{ config
, lib
, pkgs
, ...
}:
with lib;
let
  cfg = config.workstation.system;

  withEFI = cfg.bootloader.efiSupport;
  withSystemdBoot = cfg.bootloader.flavour == "systemd-boot";
  withGrub = cfg.bootloader.flavour == "grub";
in
{
  options = {
    workstation = {
      system = {
        bootloader = {
          efiSupport = mkOption {
            type = types.bool;
            default = true;
            example = "false";
            description = ''
              Whether to enable EFI support.
            '';
          };

          efiSysMountPoint = mkOption {
            type = types.str;
            default = "/boot";
            example = "/boot/efi";
            description = ''
              Where the EFI System Partition is mounted.
            '';
          };

          flavour = mkOption {
            type = types.enum
              [
                "systemd-boot"
              ];
            default = "systemd-boot";
            description = ''
              The bootloader flavour.
            '';
          };

          secureboot = mkOption {
            type = types.bool;
            default = false;
            example = "true";
            description = ''
              Whether to enable Secure Boot support.
            '';
          };
        };
      };
    };
  };

  config = mkMerge
    [
      (
        mkIf withEFI
          {
            boot = {
              loader = {
                efi = {
                  canTouchEfiVariables = true;
                  efiSysMountPoint = cfg.bootloader.efiSysMountPoint;
                };
              };
            };
          }
      )
      (
        mkIf withSystemdBoot
          {
            workstation = {
              bootloaders = {
                systemd-boot = {
                  enable = true;
                };
              };
            };
          }
      )
      (
        mkIf cfg.bootloader.secureboot
          {
            boot = {
              lanzaboote =
                assert withSystemdBoot;
                {
                  enable = true;
                  pkiBundle = "/etc/secureboot";
                };
            };
          }
      )
    ];
}
