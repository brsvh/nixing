{
  nonfree-fonts =
    final: prev:
    let
      inherit (prev) callPackage;
    in
    {
      foundertype-fonts = callPackage ./foundertype-fonts/package.nix { };

      tsangertypeFonts = callPackage ./tsangertype-fonts/package.nix { };
    };
}
