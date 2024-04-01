{ config
, lib
, pkgs
, ...
}:
with builtins;
with lib;
{
  home = {
    packages = with pkgs;
      [
        agrep
        findutils
        gnugrep
        jq
        obsidian
        ripgrep
      ];
  };

  programs = {
    command-not-found = {
      enable = true;
    };

    fzf = {
      enable = true;
      enableBashIntegration = config.programs.bash.enable;
      enableFishIntegration = config.programs.fish.enable;
      enableZshIntegration = config.programs.zsh.enable;
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
  };
}
