{
  my,
  pkgs,
  ...
}:
{
  imports = [
    my.nixosModules.fonts
    my.nixosModules.i18n
    my.nixosProfiles.fonts
  ];

  fonts = {
    fontconfig = {
      emoji = {
        enable = true;
      };

      languages = {
        chinese = {
          enable = true;
        };
      };

      symbol = {
        enable = true;
      };
    };
  };

  i18n = {
    inputMethod = {
      fcitx5 = {
        addons = with pkgs; [
          fcitx5-rime
        ];
      };

      ibus = {
        chinese = {
          enable = true;
          engine = pkgs.ibus-engines.rime;
        };
      };
    };

    languages = {
      chinese = {
        enable = true;
      };
    };
  };
}
