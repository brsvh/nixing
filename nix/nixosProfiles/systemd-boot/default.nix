{
  lib,
  ...
}:
let
  inherit (lib)
    mkDefault
    ;
in
{
  boot = {
    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = mkDefault "/boot/efi";
      };

      systemd-boot = {
        enable = true;
      };
    };
  };
}
