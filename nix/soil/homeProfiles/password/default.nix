{
  cell,
  config,
  inputs,
  pkgs,
}:
let
  projectRoot = inputs.self + "/.";
in
{
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
        source = projectRoot + "/etc/password-store";
        recursive = true;
      };
    };
  };
}
