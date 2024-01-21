{ config
, lib
, pkgs
, ...
}:
with lib;
{
  imports =
    [
      ./chinese.nix
      ./english.nix
    ];

  config =
    {
      fonts = {
        fontconfig = {
          enable = true;
        };
      };
    };
}
