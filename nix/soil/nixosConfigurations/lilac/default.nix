{
  cell,
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  inherit (inputs)
    emacs-overlay
    hardware
    hercules-ci-agent
    lanzaboote
    nix-alien
    ;

  inherit (inputs.cells) apps fonts my-emacs;

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
    cell.nixosProfiles.docker
    cell.nixosProfiles.hercules-ci-agent
    cell.nixosProfiles.libvirt
    cell.nixosProfiles.modules
    cell.nixosSecrets.lilac
    cell.nixosSuites.gnome-workstation
    cell.nixosSuites.laptop
    cell.nixosUsers.root
    cell.nixosUsers.changbingshan
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
        apps.overlays.unfree
        emacs-overlay.overlays.default
        fonts.overlays.proprius-fonts
        hercules-ci-agent.overlays.default
        lanzaboote.overlays.default
        my-emacs.overlays.emacs
        nix-alien.overlays.default
      ];
    };
  };

  boot =
    let
      kernelPackages = pkgs.linuxPackages_zen;
    in
    {
      inherit kernelPackages;

      binfmt = {
        emulatedSystems = [
          "aarch64-linux"
          "riscv64-linux"
        ];
      };

      extraModprobeConfig = ''
        options kvm_intel nested=1
        options v4l2loopback devices=1 video_nr=1 card_label="OBS Cam" exclusive_caps=1
      '';

      extraModulePackages = with kernelPackages; [ v4l2loopback ];

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

      kernelModules = [
        "kvm-intel"
        "v4l2loopback"
      ];

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

    graphics = {
      enable = true;
    };

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

    hercules-ci-agent = {
      settings = {
        binaryCachesPath = config.sops.secrets."hercules-ci/binary-caches.json".path;
        clusterJoinTokenPath = config.sops.secrets."hercules-ci/cluster-join-token.key".path;
      };
    };

    xserver = {
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
    stateVersion = "24.11";
  };

  time = {
    timeZone = "Asia/Shanghai";
  };

  virtualisation = {
    docker = {
      storageDriver = "btrfs";
    };
  };
}
