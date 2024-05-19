let
  name = "linux";
in
{
  inherit name;

  content = {
    extraArgs = [ "-f" ];

    subvolumes = {
      "/gnu" = {
        mountOptions = [
          "compress=zstd"
          "noatime"
          "ssd"
        ];

        mountpoint = "/gnu";
        name = "gnu";
      };
      "/home" = {
        mountOptions = [
          "compress=zstd:1"
          "ssd"
        ];

        mountpoint = "/home";
        name = "home";
      };
      "/nix" = {
        mountOptions = [
          "compress=zstd:1"
          "noatime"
          "ssd"
        ];

        mountpoint = "/nix";
        name = "nix";
      };
      "/nixos" = {
        mountOptions = [
          "compress=zstd:1"
          "ssd"
        ];

        mountpoint = "/";
        name = "nixos";
      };
    };

    type = "btrfs";
  };

  size = "100%";
}
