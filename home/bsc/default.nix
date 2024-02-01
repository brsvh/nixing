{ config
, lib
, pkgs
, ...
}:
with builtins;
with lib;
{
  imports =
    [
      ./browser.nix
      ./desktop.nix
      ./emacs.nix
      ./fonts.nix
      ./gpg.nix
      ./input-method.nix
      ./mail.nix
      ./nix.nix
      ./password.nix
      ./shell.nix
      ./ssh.nix
      ./TeX.nix
      ./tools.nix
    ];

  home = {
    fullname = "Burgess Chang";

    packages = with pkgs;
      [
        cachix
      ];

    sessionPath =
      [
        "${config.home.sessionVariables.XDG_BIN_HOME}"
      ];

    sessionVariables = {
      XDG_BIN_HOME = "${config.home.homeDirectory}/.local/bin";
    };
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
