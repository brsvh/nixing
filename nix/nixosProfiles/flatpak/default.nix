{
  inputs,
  ...
}:
let
  inherit (inputs)
    nix-flatpak
    ;
in
{
  imports = [
    nix-flatpak.nixosModules.nix-flatpak
  ];

  services = {
    flatpak = {
      enable = true;
    };
  };
}
