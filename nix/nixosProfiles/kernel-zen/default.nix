{
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    mkForce
    ;
in
{
  boot = {
    kernelPackages = mkForce pkgs.linuxPackages_zen;
  };
}
