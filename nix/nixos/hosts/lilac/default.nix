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
    hardware.nixosModules.common-gpu-nvidia-nonprime
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
        "ahci"
        "ehci_pci"
        "nvme"
        "rtsx_pci_sdmmc"
        "sd_mod"
        "sdhci_pci"
        "uas"
        "usb_storage"
        "usbhid"
        "xhci_pci"
      ];
    };

    kernelPackages = pkgs.linuxPackages_zen;

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

    opengl = {
      driSupport32Bit = lib.mkDefault true;
    };
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
    };
  };

  system = {
    stateVersion = "24.05";
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
}
