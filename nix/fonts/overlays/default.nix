{
  proprius-fonts =
    final: prev:
    let
      inherit (prev) callPackage;
    in
    {
      alibaba-puhuiti = callPackage ./alibaba-puhuiti/package.nix { };

      foundertype-fonts = callPackage ./foundertype-fonts/package.nix { };

      tsangertypeFonts = callPackage ./tsangertype-fonts/package.nix { };
    };
}
