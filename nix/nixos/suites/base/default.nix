{ cell, modulesPath, ... }:
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    cell.nixosProfiles.fish
    cell.nixosProfiles.nix
    cell.nixosProfiles.openssh
  ];
}
