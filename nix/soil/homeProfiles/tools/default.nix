{ config, lib, ... }:
{
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
