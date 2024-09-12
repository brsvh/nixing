{
  emacs,
  pkgs,
  epkgs,
  ...
}:
let
  inherit (pkgs) callPackage;

  callPackage' = pkg: callPackage pkg { inherit emacs epkgs; };
in
{
  eglot-booster = callPackage' ./eglot-booster;

  on = callPackage' ./on;

  pangu-spacing = callPackage' ./pangu-spacing;

  sideline-eldoc = callPackage' ./sideline-eldoc;
}
