{
  lib,
  ...
}:
let
  inherit (lib)
    makeExtensible
    ;

  shelf-lib = makeExtensible (
    final:
    let
      callLibs =
        file:
        import file {
          inherit
            lib
            ;

          shelf-lib = final;
        };
    in
    {
      collectors = callLibs ./collectors.nix;

      importers = callLibs ./importers.nix;
    }
  );
in
shelf-lib
