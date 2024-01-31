{ config
, lib
, pkgs
, ...
}:
with builtins;
with lib;
{
  services = {
    guix = {
      enable = true;

      gc = {
        dates = "monthly";
        enable = true;
        extraArgs =
          [
            "--delete-generation"
            "--optimize"
          ];
      };
    };
  };
}
