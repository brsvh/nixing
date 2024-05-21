{
  config,
  inputs,
  lib,
  modulesPath,
  pkgs,
  ...
}:
let
  inherit (inputs) disko hardware;
  inherit (inputs.cells) my-emacs unfree;

  # This device will not be exposed to the public network. The domain
  # name setting is fake, solely to automatically configure the correct
  # email domain in some software.
  domainName = "iscas.ac.cn";

  hostName = "lilac";

  system = "x86_64-linux";
in
{
  imports = [
    cell.nixosProfiles.dae
    # REVIEW re-enable after upstream compatibility with Cachix 1.7.3.
    # cell.nixosProfiles.hercules-ci-agent
    cell.nixosSecrets.lilac
    cell.nixosSuites.gnome-workstation
    cell.nixosSuites.laptop
    cell.nixosUsers.root
    cell.nixosUsers.changbingshan
    disko.nixosModules.disko
    hardware.nixosModules.common-cpu-intel
  ];

  bee = {
    inherit system;

    home = inputs.home-manager-unstable;

    pkgs = import inputs.nixos-unstable {
      inherit system;

      config = {
        allowUnfree = true;
      };

      overlays = [
        my-emacs.overlays.emacs
        unfree.overlays.unfree
      ];
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

      verbose = false;
    };

    kernelModules = [ "kvm-intel" ];
    kernelPackages = pkgs.linuxPackages_zen;
    kernelParams = [
      "quiet"
      "loglevel=3"
      "systemd.show_status=auto"
      "rd.udev.log_level=3"
    ];

    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
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
    systemPackages = with pkgs; [ config.bee.home.packages.home-manager ];
  };

  hardware = {
    enableRedistributableFirmware = lib.mkDefault true;

    nvidia = {
      modesetting = {
        enable = true;
      };

      open = true;

      powerManagement = {
        enable = false;
        finegrained = false;
      };

      prime = {
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:1:0:0";

        offload = {
          enable = true;
          enableOffloadCmd = true;
        };

        reverseSync = {
          enable = true;
        };
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
  };

  networking = {
    inherit hostName;

    domain = domainName;

    fqdn = domainName;
  };

  services = {
    dae = {
      configFile = config.sops.secrets."dae/config.dae".path;
    };

    displayManager = {
      defaultSession = "gnome-xorg";
    };

    # REVIEW re-enable after upstream compatibility with Cachix 1.7.3.
    # hercules-ci-agent = {
    #   settings = {
    #     binaryCachesPath = config.sops.secrets."hercules-ci/binary-caches.json".path;
    #     clusterJoinTokenPath = config.sops.secrets."hercules-ci/cluster-join-token.key".path;
    #   };
    # };

    xserver = {
      displayManager = {
        gdm = {
          wayland = false;
        };
      };

      videoDrivers = [ "nvidia" ];
    };
  };

  swapDevices = [
    {
      device = "/var/lib/swapfile";
      size = 16 * 1024;
    }
  ];

  system = {
    stateVersion = "24.05";
  };

  time = {
    timeZone = "Asia/Shanghai";
  };
}
