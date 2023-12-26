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
      [ ];

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
    any-nix-shell = {
      enable = true;
      enableFishIntegration = true;
    };

    emacs = {
      enable = true;
      package = pkgs.emacs-pgtk;
    };

    firefox = {
      enable = true;
    };

    fish = {
      enable = true;
      interactiveShellInit = ''
        set fish_greeting
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

    gpg = {
      enable = true;
      homedir = "${config.xdg.stateHome}/gnupg";
    };

    home-manager = {
      enable = true;
      path = mkForce "${home-manager}";
    };
  };

  services = {
    gpg-agent = {
      enable = true;
      enableBashIntegration = config.programs.bash.enable;
      enableExtraSocket = true;
      enableSshSupport = true;
      enableFishIntegration = config.programs.fish.enable;
      pinentryFlavor = "gnome3";
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
