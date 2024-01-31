{ config
, lib
, pkgs
, ...
}:
with builtins;
with lib;
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
      enableSshSupport = true;
      enableFishIntegration = config.programs.fish.enable;
      pinentryFlavor = "gnome3";
    };
  };
}
