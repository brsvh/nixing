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

  boot = {
    extraModulePackages = [ ];
    kernelModules = [ "kvm-intel" ];
    initrd = {
      availableKernelModules = [
        "xhci_pci"
        "thunderbolt"
        "nvme"
        "usb_storage"
        "sd_mod"
      ];
      kernelModules = [ ];
    };

    lanzaboote = {
      enable = true;
      pkiBundle = "/etc/secureboot";
    };

    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };
      systemd-boot = {
        enable = mkForce false;
      };
    };

    plymouth = {
      enable = true;
      theme = "bgrt";
    };
  };

  console = {
    earlySetup = true;
    font = "eurlatgr";
    keyMap = mkDefault "us";
    useXkbConfig = true;
  };

  environment = {
    sessionVariables = {
      # XDG Base Directory Specification
      XDG_CACHE_HOME = "$HOME/.cache";
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME = "$HOME/.local/share";
      XDG_STATE_HOME = "$HOME/.local/state";

      # This is not part of XDG Base Directory Specification, although
      # the naming looks like it.
      XDG_BIN_HOME = "$HOME/.local/bin";
    };

    systemPackages = with pkgs; [
      any-nix-shell
      emacs-pgtk
      ((emacsPackagesFor emacs-pgtk).emacsWithPackages
        (
          epkgs:
            with epkgs;
            [
              nix-mode
            ]
        ))
      fish
      gnome.adwaita-icon-theme
    ];

    variables = {
      "EDITOR" = "emacs";
    };

    profiles = [
      # REVIEW remove this after NixOS/nixpkgs#241518 merge.
      (
        mkIf
          config.nix.settings.use-xdg-base-directories
          "\${XDG_STATE_HOME:-$HOME/.local/state}/nix/profile"
      )
    ];
  };

  hardware = {
    pulseaudio = {
      enable = mkForce (! config.services.pipewire.enable);
    };
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales =
      [
        "C.UTF-8/UTF-8"

        "zh_CN.UTF-8/UTF-8"
        "zh_HK.UTF-8/UTF-8"
        "zh_SG.UTF-8/UTF-8"
        "zh_TW.UTF-8/UTF-8"

        "zh_HK/BIG5-HKSCS"
        "zh_TW/BIG5"

        "zh_TW.EUC-TW/EUC-TW"

        "zh_CN.GB18030/GB18030"

        "zh_CN/GB2312"
        "zh_SG/GB2312"

        "zh_CN.GBK/GBK"
        "zh_SG.GBK/GBK"

        "en_AU.UTF-8/UTF-8"
        "en_BW.UTF-8/UTF-8"
        "en_CA.UTF-8/UTF-8"
        "en_DK.UTF-8/UTF-8"
        "en_GB.UTF-8/UTF-8"
        "en_HK.UTF-8/UTF-8"
        "en_IE.UTF-8/UTF-8"
        "en_IL/UTF-8"
        "en_IN/UTF-8"
        "en_NG/UTF-8"
        "en_NZ.UTF-8/UTF-8"
        "en_PH.UTF-8/UTF-8"
        "en_SC.UTF-8/UTF-8"
        "en_SG.UTF-8/UTF-8"
        "en_US.UTF-8/UTF-8"
        "en_ZA.UTF-8/UTF-8"
        "en_ZM/UTF-8"
        "en_ZW.UTF-8/UTF-8"

        "en_AU/ISO-8859-1"
        "en_BW/ISO-8859-1"
        "en_CA/ISO-8859-1"
        "en_DK/ISO-8859-1"
        "en_GB/ISO-8859-1"
        "en_HK/ISO-8859-1"
        "en_IE/ISO-8859-1"
        "en_NZ/ISO-8859-1"
        "en_PH/ISO-8859-1"
        "en_SG/ISO-8859-1"
        "en_US/ISO-8859-1"
        "en_ZA/ISO-8859-1"
        "en_ZW/ISO-8859-1"

        "en_IE@euro/ISO-8859-15"

        "ja_JP.UTF-8/UTF-8"

        "ja_JP.EUC-JP/EUC-JP"

        "ko_KR.UTF-8/UTF-8"

        "ko_KR.EUC-KR/EUC-KR"
      ];
  };

  networking = {
    networkmanager = {
      enable = true;
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
        export HISTFILE=''${XDG_CACHE_HOME}/''${_bash_type}/history
        mkdir -p $(dirname $HISTFILE)
      '';
    };

    dconf = {
      enable = true;
    };

    fish = {
      enable = true;
      interactiveShellInit = ''
        set fish_greeting
        any-nix-shell fish --info-right | source
      '';
    };
  };

  security = {
    polkit = {
      enable = true;
    };

    rtkit = {
      enable = true;
    };
  };

  services = {
    openssh = {
      enable = true;
    };

    pipewire = {
      alsa = {
        enable = true;
        support32Bit = true;
      };

      enable = true;

      jack = {
        enable = true;
      };

      pulse = {
        enable = true;
      };
    };

    udev = {
      packages = with pkgs; [
        gnome.gnome-settings-daemon
      ];
    };

    xserver = {
      desktopManager = {
        gnome = {
          enable = true;
        };
      };

      displayManager = {
        gdm = {
          enable = true;
        };
      };

      enable = true;
      layout = "us";

      libinput = {
        enable = true;
      };

      videoDrivers = [ "modesetting" ];
    };
  };

  swapDevices = [
    {
      device = "/var/lib/swapfile";
      size = 16 * 1024;
    }
  ];

  time = {
    timeZone = "Asia/Shanghai";
  };

  users = {
    defaultUserShell = pkgs.fish;
    users = {
      bsc = {
        isNormalUser = true;
        description = "Burgess Chang";
        extraGroups = [ "wheel" "networkmanager" ];
      };
    };
  };

  zramSwap = {
    enable = true;
    algorithm = "zstd";
    memoryPercent = 100;
  };
}
