{
  config,
  lib,
  pkgs,
  ...
}:
{
  environment = {
    systemPackages = with pkgs; [
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

    git = {
      enable = true;

      lfs = {
        enable = true;
      };
    };

    nix-index = {
      enable = true;
      enableBashIntegration = lib.mkDefault true;
      enableFishIntegration = lib.mkDefault config.programs.fish.enable;
      enableZshIntegration = lib.mkDefault config.programs.zsh.enable;
    };

    nix-ld = {
      enable = true;
    };
  };
}
