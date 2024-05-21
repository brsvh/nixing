{ inputs, ... }:
let
  inherit (inputs.cells) my-emacs;
in
{
  imports = [ my-emacs.homeModules.my-emacs ];

  programs = {
    my-emacs = {
      enable = true;
    };
  };
}
