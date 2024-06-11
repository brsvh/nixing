{
  config,
  lib,
  pkgs,
  ...
}:
{
  home = {
    packages = with pkgs; [
      # debug
      cntr

      # development
      jq

      # network
      curl

      # search
      agrep
      fd
      findutils
      gnugrep
      ripgrep
    ];
  };

  programs = {
    command-not-found = {
      enable = false;
    };

    nix-index = {
      enable = true;
      enableBashIntegration = lib.mkDefault true;
      enableFishIntegration = lib.mkDefault config.programs.fish.enable;
      enableZshIntegration = lib.mkDefault config.programs.zsh.enable;
    };
  };
}
