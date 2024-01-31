{ config
, lib
, pkgs
, ...
}:
with builtins;
with lib;
{
  sops = {
    age = {
      keyFile = "/var/lib/sops/key.txt";
      generateKey = true;
      sshKeyPaths =
        [
          "/etc/ssh/ssh_host_ed25519_key"
        ];
    };

    defaultSopsFile = ./secrets.yaml;

    secrets = {
      "dae/config.dae" = {
        restartUnits = [ "dae.service" ];
      };
    };
  };
}
