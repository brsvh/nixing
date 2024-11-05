{
  lib,
  ...
}:
let
  inherit (builtins)
    pathExists
    readDir
    ;

  inherit (lib)
    filterAttrs
    hasSuffix
    mapAttrs'
    removeSuffix
    ;
in
rec {
  rakeLeaves =
    dirPath:
    let
      seive =
        n: t:
        let
          isDirectory = t == "directory";

          isNixFile = t == "regular" && hasSuffix ".nix" n;
        in
        isNixFile || isDirectory;

      collect = n: t: {
        name = removeSuffix ".nix" n;
        value =
          let
            node = dirPath + /${n};

            isDirectory = t == "directory";

            isNixFile = t == "regular" && hasSuffix ".nix" n;

            hasDefault = isDirectory && pathExists (/${node} + /default.nix);
          in
          if isNixFile || hasDefault then node else rakeLeaves node;
      };

      files = filterAttrs seive (readDir dirPath);
    in
    filterAttrs (n: v: v != { }) (mapAttrs' collect files);
}
