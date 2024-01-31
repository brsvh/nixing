{ config
, lib
, pkgs
, ...
}:
with builtins;
with lib;
{
  users = {
    users = {
      bsc = {
        isNormalUser = true;
        description = "Burgess Chang";
        extraGroups =
          [
            "audio"
            "jackaudio"
            "wheel"
            "networkmanager"
          ];
      };
    };
  };
}
