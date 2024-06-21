{ inputs, ... }:
let
  inherit (inputs) nix-index-database sops;

  inherit (inputs.cells) fonts my-emacs;
in
{
  imports = [
    fonts.homeModules.fonts
    fonts.homeModules.tsangertype-fonts
    my-emacs.homeModules.my-emacs
    nix-index-database.hmModules.nix-index
    sops.homeManagerModules.sops
  ];
}
