{ config, ... }:
{
  programs = {
    direnv = {
      enable = true;
      enableBashIntegration = config.programs.bash.enable;
      enableFishIntegration = config.programs.fish.enable;

      nix-direnv = {
        enable = true;
      };
    };
  };
}
