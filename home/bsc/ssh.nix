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
    ssh = {
      enable = true;

      includes = [ "isrc/*" ];

      matchBlocks = {
        "github.com" = {
          hostname = "ssh.github.com";
          user = "git";
          port = 443;
        };
      };
    };
  };
}
