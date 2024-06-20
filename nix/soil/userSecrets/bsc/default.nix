{ cell, config, ... }:
{
  imports = [ cell.homeProfiles.ssh ];

  sops = {
    age = {
      keyFile = "${config.xdg.configHome}/sops/key.txt";
      generateKey = true;
      sshKeyPaths = [ "${config.home.homeDirectory}/.ssh/id_ed25519" ];
    };

    defaultSopsFile = ./secrets.yaml;

    secrets = {
      "google/api.conf" = {
        mode = "0440";
        path = "${config.xdg.configHome}/environment.d/90-google.conf";
      };
    };
  };
}
