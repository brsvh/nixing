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
      any-nix-shell
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
      fish
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

  programs = {
    bash = {
      interactiveShellInit = ''
        case :$SHELLOPTS: in
          *:posix:*)
            _bash_type="sh"
            ;;
          *)
            _bash_type="bash"
            ;;
        esac
        export HISTFILE=''${XDG_CACHE_HOME:-$HOME/.cache}/''${_bash_type}/history
        mkdir -p $(dirname $HISTFILE)
      '';
    };

    fish = {
      enable = true;
      interactiveShellInit = ''
        set fish_greeting
        any-nix-shell fish --info-right | source
      '';
    };
  };

  services = {
    openssh = {
      enable = true;
    };

    xserver = {
      videoDrivers = [ "modesetting" ];
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
    defaultUserShell = pkgs.fish;
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
      proxy = {
        client = {
          config = config.sops.secrets."dae/config.dae".path;
          enable = true;
          flavour = "dae";
        };
      };
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
  };
}
