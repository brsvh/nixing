final: prev:
let
  inherit (prev.lib)
    packagesFromDirectoryRecursive
    ;
in
packagesFromDirectoryRecursive {
  inherit (prev)
    callPackage
    ;

  directory = ../../packages/fonts;
}
