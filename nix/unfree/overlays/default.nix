{
  unfree =
    final: prev:
    let
      inherit (prev) callPackage;
    in
    {
      wemeet = callPackage ./wemeet/package.nix { };
    };
}
