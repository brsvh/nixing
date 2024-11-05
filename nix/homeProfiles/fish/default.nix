{
  config,
  lib,
  my,
  ...
}:
let
  inherit (builtins)
    concatStringsSep
    ;

  inherit (lib)
    mapAttrsToList
    mkAfter
    ;

  vars =
    let
      inherit (config.home)
        sessionVariables
        ;

      list = mapAttrsToList (n: v: "set -gx ${n} \"${v}\"") sessionVariables;
    in
    concatStringsSep "\n" list;
in
{
  imports = [
    my.homeProfiles.envvars
  ];

  programs = {
    fish = {
      enable = true;

      interactiveShellInit = mkAfter ''
        set fish_greeting

        ${vars}
      '';
    };
  };
}
