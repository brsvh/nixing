{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  inherit (inputs)
    nix-index-database
    ;

  inherit (lib)
    mkDefault
    ;
in
{
  imports = [
    nix-index-database.nixosModules.nix-index
  ];

  environment = {
    systemPackages = with pkgs; [
      # network
      curl
      dogdns

      # nix
      nix-alien
      nix-output-monitor
      nix-tree
      nvd

      # search
      agrep
      fd
      findutils
      gnugrep
      ripgrep

      # terminal multiplexer
      screen
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
      enableBashIntegration = mkDefault true;
      enableFishIntegration = mkDefault config.programs.fish.enable;
      enableZshIntegration = mkDefault config.programs.zsh.enable;
    };

    nix-ld = {
      enable = true;
    };

    tmux = {
      enable = true;
      keyMode = "emacs";
      shortcut = "'";
    };
  };
}
