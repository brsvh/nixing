{ config
, lib
, pkgs
, pkgs-stable
, ...
}:
with builtins;
with lib;
{
  environment = {
    systemPackages = with pkgs;
      [
        ibm-plex
        sbctl
      ];
  };

  workstation = {
    audio = {
      system = "pipewire";
      enableAlsa = true;
      enableJack = true;
    };

    desktop = {
      flavour = "plasma6";

      fonts = {
        sansFontName = "IBM Plex Sans";
        serifFontName = "IBM Plex Serif";
        monoFontName = "IBM Plex Mono";
      };

      keyboardLayout = "us";
    };

    networking = {
      manager = "network-manager";

      openssh = {
        enable = true;
      };

      proxy = {
        client = {
          config = config.sops.secrets."dae/config.dae".path;
          enable = true;
          flavour = "dae";
        };

        dae = pkgs-stable.dae;
        singbox = pkgs-stable.sing-box;
      };
    };

    shell = {
      flavour = "fish";
    };

    system = {
      bootloader = {
        efiSupport = true;
        efiSysMountPoint = "/boot/efi";
        flavour = "systemd-boot";
        secureboot = true;
      };

      console = {
        font = "eurlatgr";
        keymap = "us";
      };

      i18n = {
        english = {
          enable = true;
          primary = true;
        };

        chinese = {
          enable = true;
        };
      };

      initrd = {
        modules = {
          implication =
            [
              "xhci_pci"
              "thunderbolt"
              "nvme"
              "usb_storage"
              "sd_mod"
            ];
        };
      };

      kernel = {
        package = pkgs.linuxPackages_zen;
        modules =
          [
            "kvm-intel"
          ];
      };

      startup = {
        quiet = true;
      };

      swap = {
        enable = true;
        devices =
          [
            {
              device = "/var/lib/swapfile";
              size = 16 * 1024;
            }
          ];
      };

      zram = {
        enable = true;
        percent = 100;
      };
    };

    video = {
      drivers =
        [
          "modesetting"
          "fbdev"
        ];
    };
  };
}
