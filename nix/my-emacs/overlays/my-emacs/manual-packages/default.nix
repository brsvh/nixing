{
  emacs,
  passedPackages,
  pkgs,
  trivialBuild,
}:
let
  inherit (pkgs) callPackage;
in
{
  on = callPackage ./on { inherit trivialBuild; };
}
