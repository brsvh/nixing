{
  config,
  lib,
  pkgs,
  ...
}:
with builtins;
with lib;
let
  cfg = config.home;
in
{
  options.home = {
    fullname = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        The full name of home.username.
      '';
    };
  };
}
