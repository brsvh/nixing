{ inputs, cell }:
let
  inherit (inputs) lib nixpkgs;

  callPackage = pkg: lib.callPackageWith nixpkgs pkg;

  callPackage' = pkg: callPackage pkg { };

  findPackage =
    dir:
    with lib;
    let
      pathToPackage = subdir: dir + "/${subdir}/package.nix";

      subdirs = filterAttrs (_: type: type == "directory") (readDir dir);
    in
    mapAttrs (name: _: pathToPackage name) subdirs;
in
lib.mapAttrs (_: pkg: callPackage' pkg) (findPackage ./.)
