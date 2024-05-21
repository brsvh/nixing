{
  cell,
  inputs,
  pkgs,
  ...
}:
let
  inherit (inputs) my-emacs;
in
{
  imports = [ my-emacs.homeModules.my-emacs ];

  programs = {
    my-emacs = {
      enable = true;
    };
  };
}
