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
    lanzaboote
    nix-alien
    ;

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
    cell.nixosProfiles.libvirt
    cell.nixosProfiles.modules
    cell.nixosSecrets.eustoma
    cell.nixosSuites.plasma-workstation
    cell.nixosSuites.laptop
    cell.nixosUsers.root
    cell.nixosUsers.bsc
    hardware.nixosModules.lenovo-thinkpad-x1-nano-gen1
  ];

  bee = {
    inherit system;

    home = inputs.home-manager;

    pkgs = import inputs.nixpkgs {
      inherit system;

      config = {
        allowUnfree = true;
      };

      overlays = [
        apps.overlays.unfree
        emacs-overlay.overlays.default
        fonts.overlays.proprius-fonts
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
    systemPackages = [ config.bee.home.packages.home-manager ];
  };

  hardware = {
    enableRedistributableFirmware = lib.mkDefault true;

    graphics = {
      enable = true;
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

  nix = {
    extraOptions = ''
      !include ${config.sops.secrets."tokens/nixAccessTokens.conf".path}
    '';
  };

  services = {
    dae = {
      configFile = config.sops.secrets."dae/config.dae".path;
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
