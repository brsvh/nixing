{ config, pkgs, ... }:
{
  programs = {
    gpg = {
      enable = true;
      homedir = "${config.xdg.stateHome}/gnupg";
    };
  };

  services = {
    gpg-agent = {
      enable = true;
      enableBashIntegration = config.programs.bash.enable;
      enableExtraSocket = true;
      enableFishIntegration = config.programs.fish.enable;
      enableSshSupport = true;
    };
  };
}
