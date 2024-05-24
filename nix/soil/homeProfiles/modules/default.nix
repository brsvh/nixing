{ cell, inputs, ... }:
let
  inherit (inputs.cells) fonts my-emacs;
in
{
  imports = [
    cell.homeModules.fonts
    fonts.homeModules.tsangertype-fonts
    my-emacs.homeModules.my-emacs
  ];
}
