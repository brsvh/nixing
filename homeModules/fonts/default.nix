{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
{
  imports = [
    ./chinese.nix
    ./emoji.nix
    ./english.nix
    ./japanese.nix
    ./korean.nix
  ];

  options.fonts = {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether to use Fonts Modules.
      '';
    };

    size = mkOption {
      type = types.int;
      default = 11;
      description = ''
        The default size of fonts.
      '';
    };
  };

  config = mkIf config.fonts.enable {
    fonts = {
      fontconfig = {
        enable = true;
      };
    };
  };
}
