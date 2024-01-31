{ config
, lib
, pkgs
, ...
}:
with builtins;
with lib;
{
  programs = {
    git = {
      enable = true;
      signing = {
        key = "7B740DB9F2AC6D3B226BC53078D74502D92E0218";
        signByDefault = true;
      };
      userEmail = "bsc@brsvh.org";
      userName = "Burgess Chang";
    };
  };
}
