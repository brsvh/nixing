{
  config,
  hardware,
  lib,
  modulesPath,
  pkgs,
  ...
}:
with builtins;
with lib;
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    hardware.nixosModules.lenovo-thinkpad-x1-nano-gen1
    ./disko.nix
    ./guix.nix
    ./nix.nix
    ./secrets.nix
    ./tools.nix
    ./users.nix
    ./virtualisation.nix
    ./workstation.nix
  ];
}
