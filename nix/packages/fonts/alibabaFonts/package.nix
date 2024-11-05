{
  generateSplicesForMkScope,
  makeScopeWithSplicing',
  ...
}:
makeScopeWithSplicing' {
  otherSplices = generateSplicesForMkScope "alibabaFonts";
  f = (
    self:
    let
      inherit (self)
        callPackage
        ;
    in
    {
      puhuiti-2 = callPackage ./puhuiti-2.nix { };

      puhuiti-3 = callPackage ./puhuiti-3.nix { };
    }
  );
}
