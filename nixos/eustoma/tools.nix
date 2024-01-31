{ config
, lib
, pkgs
, ...
}:
with builtins;
with lib;
{
  environment = {
    systemPackages = with pkgs; [
      git
      home-manager
      jq
      nano
    ];

    variables = {
      "EDITOR" = "nano";
    };
  };
}
