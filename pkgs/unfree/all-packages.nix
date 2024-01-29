final: prev:
let
  inherit (final) callPackage;
in
{
  foundertype-fonts = callPackage ./foundertype-fonts { };
}
