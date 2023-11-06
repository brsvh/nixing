{
  disko = {
    devices = {
      disk = {
        system = {
          type = "disk";
          device = "/dev/nvme0n1";
          content = {
            type = "gpt";
            partitions = {
              esp = {
                content = {
                  format = "vfat";
                  mountpoint = "/boot/efi";
                  type = "filesystem";
                };
                name = "efi";
                priority = 1;
                size = "1G";
                type = "EF00";
              };
              bsp = {
                content = {
                  format = "ext4";
                  mountpoint = "/boot";
                  type = "filesystem";
                };
                name = "boot";
                priority = 2;
                size = "1G";
                type = "EF02";
              };
              lsp = {
                content = {
                  extraArgs = [ "-f" ];
                  subvolumes = {
                    "/gnu" = {
                      mountOptions =
                        [
                          "compress=zstd"
                          "noatime"
                          "space_cache=v1"
                          "ssd"
                        ];
                      mountpoint = "/gnu";
                      name = "gnu";
                    };
                    "/home" = {
                      mountOptions =
                        [
                          "compress=zstd:1"
                          "space_cache=v1"
                          "ssd"
                        ];
                      mountpoint = "/home";
                      name = "home";
                    };
                    "/nix" = {
                      mountOptions =
                        [
                          "compress=zstd:1"
                          "noatime"
                          "space_cache=v1"
                          "ssd"
                        ];
                      mountpoint = "/nix";
                      name = "nix";
                    };
                    "/nixos" = {
                      mountOptions =
                        [
                          "compress=zstd:1"
                          "space_cache=v1"
                          "ssd"
                        ];
                      mountpoint = "/";
                      name = "nixos";
                    };
                  };
                  type = "btrfs";
                };
                size = "100%";
                name = "linux";
              };
            };
          };
        };
      };
    };
  };
}
