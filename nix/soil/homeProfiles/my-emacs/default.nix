{ inputs, ... }:
let
  inherit (inputs.cells) my-emacs;
in
{
  programs = {
    my-emacs = {
      defaultEditor = true;
      enable = true;
    };
  };
}
