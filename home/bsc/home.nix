{ config
, home-manager
, lib
, pkgs
, ...
}:
with lib;
{
  home = {
    packages = with pkgs;
      [
        any-nix-shell
      ];

    sessionPath =
      [
        "${config.home.sessionVariables.XDG_BIN_HOME}"
      ];

    sessionVariables = {
      XDG_BIN_HOME = "${config.home.homeDirectory}/.local/bin";
    };
  };

  nix = {
    package = pkgs.nixUnstable;
    settings = {
      experimental-features =
        [
          "ca-derivations"
          "flakes"
          "nix-command"
          "repl-flake"
        ];
      use-xdg-base-directories = true;
    };
  };

  programs = {
    emacs = {
      enable = true;
      package = pkgs.emacs-pgtk;
    };

    fish = {
      enable = true;
      interactiveShellInit = ''
        set fish_greeting
        any-nix-shell fish --info-right | source
      '';
    };

    git = {
      enable = true;
      signing = {
        key = "7B740DB9F2AC6D3B226BC53078D74502D92E0218";
        signByDefault = true;
      };
      userEmail = "bsc@brsvh.org";
      userName = "Burgess Chang";
    };

    home-manager = {
      enable = true;
      path = mkForce "${home-manager}";
    };
  };

  xdg = {
    enable = true;
    userDirs = {
      enable = true;
      createDirectories = true;
      desktop = "${config.xdg.dataHome}/desktop";
      documents = "${config.xdg.dataHome}/documents";
      download = "${config.xdg.dataHome}/downloads";
      music = "${config.xdg.dataHome}/music";
      pictures = "${config.xdg.dataHome}/pictures";
      publicShare = "${config.xdg.dataHome}/public";
      templates = "${config.xdg.dataHome}/templates";
      videos = "${config.xdg.dataHome}/videos";
    };
  };
}
