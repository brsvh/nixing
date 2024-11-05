{
  my,
  ...
}:
{
  imports = [
    my.nixosProfiles.envvars
  ];

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
