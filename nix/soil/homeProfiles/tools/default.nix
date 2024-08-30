{
  config,
  lib,
  pkgs,
  ...
}:
{
  home = {
    packages = with pkgs; [
      # calculator
      eva

      # debug
      cntr

      # development
      jq

      # diff
      delta

      # file
      bat
      choose
      eza
      hex
      rnr

      # network
      curl
      dogdns

      # process
      procs

      # search
      agrep
      fd
      findutils
      fzf
      gnugrep
      # igrep
      ripgrep

      # terminal
      screen
      zellij
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
