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
      ./emoji.nix
      ./english.nix
    ];

  options.fonts = {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to use Fonts Modules.
      '';
    };
  };

  config = mkIf config.fonts.enable
    {
      fonts = {
        fontconfig = {
          enable = true;
        };
      };
    };
}
