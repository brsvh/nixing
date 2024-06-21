{ cell, config, ... }:
{
  imports = [ cell.homeProfiles.ssh ];

  # FIXME: it is necesscary to start sops-nix.service manually, too bad.
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
      };
    };
  };
}
