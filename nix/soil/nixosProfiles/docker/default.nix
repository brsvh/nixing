{ cell, ... }:
{
  imports = [ cell.nixosProfiles.xdg ];

  environment = {
    sessionVariables = {
      DOCKER_CONFIG = "$XDG_CONFIG_HOME/docker";
    };
  };

  virtualisation = {
    docker = {
      enable = true;
    };
  };
}
