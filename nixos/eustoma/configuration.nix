{ config
, hardware
, lib
, modulesPath
, pkgs
, ...
}:
with builtins;
with lib;
{
  imports =
    [
      (modulesPath + "/installer/scan/not-detected.nix")
      hardware.nixosModules.lenovo-thinkpad-x1-nano-gen1
    ];

  environment = {
    systemPackages = with pkgs; [
      emacs-pgtk
      (
        (emacsPackagesFor emacs-pgtk).emacsWithPackages
          (
            epkgs:
              with epkgs;
              [
                nix-mode
              ]
          )
      )
      git
      home-manager
      sbctl
    ];

    variables = {
      "EDITOR" = "emacs";
    };
  };

  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = mkDefault ''
        --delete-older-than 4w
      '';
    };

    optimise = {
      automatic = true;
    };

    settings = {
      allowed-users =
        [
          "@users"
        ];

      experimental-features =
        [
          "ca-derivations"
          "flakes"
          "nix-command"
          "repl-flake"
        ];

      sandbox = true;

      trusted-users =
        [
          "@admin"
          "@wheel"
          "root"
        ];

      use-xdg-base-directories = true;
    };
  };

  sops = {
    age = {
      keyFile = "/var/lib/sops/key.txt";
      generateKey = true;
      sshKeyPaths =
        [
          "/etc/ssh/ssh_host_ed25519_key"
        ];
    };
    secrets = {
      "dae/config.dae" = {
        sopsFile = ./secrets.yaml;
        restartUnits = [ "dae.service" ];
      };
    };
  };

  users = {
    users = {
      bsc = {
        isNormalUser = true;
        description = "Burgess Chang";
        extraGroups =
          [
            "audio"
            "jackaudio"
            "wheel"
            "networkmanager"
          ];
      };
    };
  };

  workstation = {
    audio = {
      system = "pipewire";
      enableAlsa = true;
      enableJack = true;
    };

    desktop = {
      flavour = "gnome3";
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
        default = "english";
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
