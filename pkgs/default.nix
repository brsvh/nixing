final: prev:
let
  inherit (final) callPackage;
in
{
  iosevka-ibm-plex-mono- = callPackage ./iosevka-ibm-plex-mono.nix { };

  iosevka-source-code-pro = callPackage ./iosevka-source-code-pro.nix { };
}
