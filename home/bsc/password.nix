{
  config,
  lib,
  pkgs,
  ...
}:
with builtins;
with lib;
{
  programs = {
    password-store = {
      enable = true;
      settings = {
        PASSWORD_STORE_DIR = "${config.xdg.dataHome}/password-store";
      };
    };
  };
}
