{
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    mkForce
    ;
in
{
  programs = {
    emacs = {
      package = mkForce pkgs.emacs30-pgtk;
    };
  };
}
