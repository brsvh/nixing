{
  config,
  inputs,
  my,
  ...
}:
let
  inherit (inputs)
    sops
    ;
in
{
  imports = [
    my.nixosProfiles.openssh
    my.nixosProfiles.dae
    my.nixosProfiles.hercules-ci-agent
    sops.nixosModules.sops
  ];

  services = {
    dae = {
      configFile = config.sops.secrets."dae/config.dae".path;
    };

    hercules-ci-agent = {
      settings = {
        staticSecretsDirectory = "/run/secrets/hercules-ci-agent";
      };
    };
  };

  sops = {
    age = {
      keyFile = "/var/lib/sops/key.txt";
      generateKey = true;
      sshKeyPaths = [
        "/etc/ssh/ssh_host_ed25519_key"
      ];
    };

    defaultSopsFile = my.etc.sops.lilac;

    secrets = {
      "dae/config.dae" = {
        restartUnits = [
          "dae.service"
        ];
      };

      "hercules-ci-agent/binary-caches.json" = {
        mode = "0440";
        owner = "hercules-ci-agent";

        restartUnits = [
          "hercules-ci-agent.service"
        ];
      };

      "hercules-ci-agent/cluster-join-token.key" = {
        mode = "0440";
        owner = "hercules-ci-agent";

        restartUnits = [
          "hercules-ci-agent.service"
        ];
      };

      "tokens/nixAccessTokens.conf" = {
        mode = "0440";
        group = "users";
      };
    };
  };
}
