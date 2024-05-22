{ config, ... }:
{
  programs = {
    direnv = {
      enable = true;
      enableBashIntegration = config.programs.bash.enable;

      nix-direnv = {
        enable = true;
      };
    };
  };
}
