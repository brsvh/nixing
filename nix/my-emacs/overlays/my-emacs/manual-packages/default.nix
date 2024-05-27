{
  emacs,
  pkgs,
  prevEpkgs,
  trivialBuild,
}:
let
  inherit (pkgs) callPackage;

  callPackage' = pkg: callPackage pkg { inherit emacs prevEpkgs; };
in
{
  eglot-booster = callPackage' ./eglot-booster;

  on = callPackage ./on { inherit trivialBuild; };
}
