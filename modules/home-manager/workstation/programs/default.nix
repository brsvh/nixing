{ config
, lib
, pkgs
, ...
}:
with lib;
{
  imports =
    [
      ./any-nix-shell.nix
    ];
}
