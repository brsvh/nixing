{ pkgs, ... }:
{
  environment = {
    systemPackages = with pkgs; [
      libguestfs
      spice
    ];
  };

  programs = {
    virt-manager = {
      enable = true;
    };
  };

  services = {
    spice-autorandr = {
      enable = true;
    };

    spice-vdagentd = {
      enable = true;
    };

    spice-webdavd = {
      enable = true;
    };
  };

  virtualisation = {
    libvirtd = {
      enable = true;

      qemu = {
        package = pkgs.qemu_kvm;
        runAsRoot = true;

        swtpm = {
          enable = true;
        };

        ovmf = {
          enable = true;

          packages = [
            (pkgs.OVMF.override {
              secureBoot = true;
              tpmSupport = true;
            }).fd
          ];
        };
      };
    };

    spiceUSBRedirection = {
      enable = true;
    };
  };
}
