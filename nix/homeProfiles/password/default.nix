{
  config,
  my,
  ...
}:
{
  home = {
    sessionVariables = {
      PASSWORD_STORE_DIR = "${config.xdg.dataHome}/password-store";
    };
  };

  programs = {
    password-store = {
      enable = true;

      settings = {
        PASSWORD_STORE_DIR = "${config.xdg.dataHome}/password-store";
      };
    };
  };

  xdg = {
    dataFile = {
      "password-store" = {
        source = my.root + "/etc/password-store";
        recursive = true;
      };
    };
  };
}
