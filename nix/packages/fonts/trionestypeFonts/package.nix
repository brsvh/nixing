{
  generateSplicesForMkScope,
  makeScopeWithSplicing',
  ...
}:
makeScopeWithSplicing' {
  otherSplices = generateSplicesForMkScope "trionestypeFonts";
  f = (
    self:
    let
      inherit (self)
        callPackage
        ;
    in
    {
      ZhuqueFangsong = callPackage ./ZhuqueFangsong.nix { };
    }
  );
}
