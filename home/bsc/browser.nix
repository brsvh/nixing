{ config
, lib
, pkgs
, ...
}:
with builtins;
with lib;
{
  programs = {
    firefox = {
      enable = true;
    };
  };
}
