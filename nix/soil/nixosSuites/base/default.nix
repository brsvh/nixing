{
  cell,
  inputs,
  modulesPath,
  ...
}:
let
  inherit (inputs) disko;
in
{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    cell.nixosProfiles.bash
    cell.nixosProfiles.fish
    cell.nixosProfiles.guix
    cell.nixosProfiles.nix
    cell.nixosProfiles.openssh
    cell.nixosProfiles.polkit
    cell.nixosProfiles.tools
    disko.nixosModules.disko
  ];
}
