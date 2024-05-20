{ config, ... }:
let
  system = "x86_64-linux";
in
{
  bee = {
    inherit system;

    home = inputs.home-manager-unstable;

    pkgs = import inputs.nixpkgs-unstable {
      inherit system;

      config = {
        allowUnfree = true;
      };
    };
  };

  home =
    let
      username = "changbingshan";
    in
    {
      inherit username;

      homeDirectory = "/home/${username}";

      sessionPath = [ "${config.home.sessionVariables.XDG_BIN_HOME}" ];

      sessionVariables = {
        XDG_BIN_HOME = "${config.home.homeDirectory}/.local/bin";
      };

      stateVersion = "24.05";
    };

  systemd = {
    user = {
      sessionVariables = config.home.sessionVariables;
    };
  };

  xdg = {
    enable = true;

    userDirs = {
      enable = true;
      createDirectories = true;
      desktop = "${config.xdg.dataHome}/Desktop";
      documents = "${config.xdg.dataHome}/Documents";
      download = "${config.xdg.dataHome}/Downloads";
      music = "${config.xdg.dataHome}/Music";
      pictures = "${config.xdg.dataHome}/Pictures";
      publicShare = "${config.xdg.dataHome}/Public";
      templates = "${config.xdg.dataHome}/Templates";
      videos = "${config.xdg.dataHome}/Videos";
    };
  };
}
