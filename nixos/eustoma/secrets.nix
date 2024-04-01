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

      "hercules-ci/binary-caches.json" = {
        mode = "0440";
        owner = "hercules-ci-agent";
        restartUnits = [ "hercules-ci-agent.service" ];
      };

      "hercules-ci/cluster-join-token.key" = {
        mode = "0440";
        owner = "hercules-ci-agent";
        restartUnits = [ "hercules-ci-agent.service" ];
      };
    };
  };
}
