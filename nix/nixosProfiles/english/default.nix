{
  my,
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
        english = {
          enable = true;
        };
      };

      symbol = {
        enable = true;
      };
    };
  };

  i18n = {
    languages = {
      english = {
        defaultLocale = "en_US.UTF-8";
        enable = true;
      };
    };
  };
}
