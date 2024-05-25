{ cell, inputs, ... }:
let
  inherit (inputs.cells) fonts home-manager my-emacs;
in
{
  imports = [
    home-manager.homeModules.fonts
    fonts.homeModules.tsangertype-fonts
    my-emacs.homeModules.my-emacs
  ];
}
