{
  environment = {
    sessionVariables = {
      NIXOS_OZONE_WL = "1";
    };
  };

  i18n = {
    inputMethod = {
      fcitx5 = {
        waylandFrontend = true;
      };
    };
  };

  services = {
    displayManager = {
      sddm = {
        wayland = {
          enable = true;
        };
      };
    };

    xserver = {
      displayManager = {
        gdm = {
          wayland = true;
        };
      };
    };
  };
}
