{
  inputs,
  lib,
  modulesPath,
  pkgs,
  ...
}:
let
  inherit (inputs) disko hardware;

  # This device will not be exposed to the public network. The domain
  # name setting is fake, solely to automatically configure the correct
  # email domain in some software.
  domainName = "iscas.ac.cn";

  hostName = "lilac";

  system = "x86_64-linux";
in
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    disko.nixosModules.disko
    hardware.nixosModules.common-cpu-intel
    hardware.nixosModules.common-gpu-nvidia-sync
  ];

  bee = {
    inherit system;

    home = inputs.home-manager-unstable;

    pkgs = import inputs.nixos-unstable {
      inherit system;

      config = {
        allowUnfree = true;
      };
    };
  };

  boot = {
    initrd = {
      availableKernelModules = [
        "nvme"
        "sd_mod"
        "thunderbolt"
        "usb_storage"
        "vmd"
        "xhci_pci"
      ];
    };

    kernelModules = [ "kvm-intel" ];
    kernelPackages = pkgs.linuxPackages_latest;

    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };

      systemd-boot = {
        enable = true;
      };
    };
  };

  console = {
    earlySetup = true;
    font = "eurlatgr";
    keyMap = lib.mkDefault "us";
  };

  disko = cell.diskoConfigurations.lilac.disko;

  environment = {
    systemPackages = with pkgs; [ gnome.adwaita-icon-theme ];
  };

  hardware = {
    enableRedistributableFirmware = lib.mkDefault true;

    nvidia = {
      modesetting = {
        enable = true;
      };

      powerManagement = {
        enable = true;
        finegrained = false;
      };

      prime = {
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:1:0:0";
      };
    };

    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = lib.mkDefault true;
    };
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";

    supportedLocales = [
      # ISO-8859-1
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

      # ISO-8859-15
      "en_IE@euro/ISO-8859-15"

      # UTF-8
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
    ];
  };

  networking = {
    inherit hostName;

    domain = domainName;

    firewall = {
      enable = true;
    };

    fqdn = domainName;

    networkmanager = {
      enable = true;
    };
  };

  nix = {
    gc = {
      automatic = true;
      dates = "weekly";

      options = lib.mkDefault ''
        --delete-older-than 4w
      '';
    };

    optimise = {
      automatic = true;
    };

    settings = {
      allowed-users = [ "@users" ];

      builders-use-substitutes = true;

      experimental-features = [
        "ca-derivations"
        "flakes"
        "nix-command"
      ];

      fallback = true;

      keep-derivations = lib.mkDefault true;

      keep-outputs = lib.mkDefault true;

      sandbox = true;

      trusted-users = [
        "@wheel"
        "root"
      ];

      use-xdg-base-directories = true;
    };
  };

  programs = {
    fish = {
      enable = true;

      interactiveShellInit = ''
        set fish_greeting
      '';
    };

    dconf = {
      enable = true;
    };
  };

  services = {
    gnome = {
      gnome-initial-setup = {
        enable = lib.mkForce false;
      };
    };

    libinput = {
      enable = true;
    };

    sshd = {
      enable = true;
    };

    udev = {
      packages = with pkgs; [ gnome.gnome-settings-daemon ];
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

      videoDrivers = [ "nvidia" ];
    };
  };

  swapDevices = [ { device = "/var/lib/swapfile"; } ];

  system = {
    stateVersion = "24.05";
  };

  time = {
    timeZone = "Asia/Shanghai";
  };

  users = {
    mutableUsers = true;

    users = {
      changbingshan = {
        description = "Bingshan Chang";

        extraGroups = [
          "audio"
          "jackaudio"
          "libvirtd"
          "wheel"
          "networkmanager"
        ];

        initialHashedPassword = "$6$cB3EK3Lynl./0Bio$bgH7P93D1lpgvEIJ3iks7Dk2IyNue7ria2aH8.xkZZ1PPooxkb7p/bEMN1UtJaV0TIeVr/eTY8oAN38vBgMKe0";
        isNormalUser = true;
        shell = pkgs.fish;
      };

      root = {
        initialHashedPassword = "$6$BnGvScsfnCRh3coN$yo.YsllSelnixuiWuiMFQYaJrneNqLrfUAhEOMw6CN/Od2kZSLdclJaa4h1TBtBP7NeWxBKsIsftFZQB46DUV.";
        shell = pkgs.fish;
      };
    };
  };

  xdg = {
    portal = {
      enable = true;
    };
  };

  zramSwap = {
    algorithm = "zstd";
    enable = true;
    memoryPercent = 100;
    priority = 5;
  };
}