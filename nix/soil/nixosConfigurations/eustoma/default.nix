{
  cell,
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  inherit (inputs) hardware hercules-ci-agent lanzaboote;
  inherit (inputs.cells) apps fonts my-emacs;

  # This device will not be exposed to the public network. The domain
  # name setting is fake, solely to automatically configure the correct
  # email domain in some software.
  domainName = "brsvh.org";

  hostName = "eustoma";

  system = "x86_64-linux";
in
{
  imports = [
    cell.nixosProfiles.dae
    cell.nixosProfiles.docker
    cell.nixosProfiles.hercules-ci-agent
    cell.nixosProfiles.libvirt
    cell.nixosSecrets.eustoma
    cell.nixosSuites.gnome-workstation
    cell.nixosSuites.laptop
    cell.nixosUsers.root
    cell.nixosUsers.bsc
    hardware.nixosModules.lenovo-thinkpad-x1-nano-gen1
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
        fonts.overlays.proprius-fonts
        hercules-ci-agent.overlays.default
        lanzaboote.overlays.default
        my-emacs.overlays.emacs
      ];
    };
  };

  boot =
    let
      kernelPackages = pkgs.linuxPackages_zen;
    in
    {
      inherit kernelPackages;

      extraModprobeConfig = ''
        options v4l2loopback devices=1 video_nr=1 card_label="OBS Cam" exclusive_caps=1
      '';

      extraModulePackages = with kernelPackages; [ v4l2loopback ];

      initrd = {
        availableKernelModules = [
          "nvme"
          "thunderbolt"
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

  disko = cell.diskoConfigurations.eustoma.disko;

  environment = {
    systemPackages = with pkgs; [ config.bee.home.packages.home-manager ];
  };

  hardware = {
    enableRedistributableFirmware = lib.mkDefault true;

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

    hercules-ci-agent = {
      settings = {
        binaryCachesPath = config.sops.secrets."hercules-ci/binary-caches.json".path;
        clusterJoinTokenPath = config.sops.secrets."hercules-ci/cluster-join-token.key".path;
      };
    };

    xserver = {
      videoDrivers = [ "modesetting" ];
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

  virtualisation = {
    docker = {
      storageDriver = "btrfs";
    };
  };
}
