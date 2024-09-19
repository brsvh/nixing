{ cell, inputs, ... }:
let
  inherit (inputs) sops;
in
{
  imports = [
    cell.nixosProfiles.openssh
    sops.nixosModules.sops
  ];

  sops = {
    age = {
      keyFile = "/var/lib/sops/key.txt";
      generateKey = true;
      sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    };

    defaultSopsFile = ./secrets.yaml;

    secrets = {
      "dae/config.dae" = {
        restartUnits = [ "dae.service" ];
      };

      "tokens/nixAccessTokens.conf" = {
        mode = "0440";
        group = "users";
      };
    };
  };
}
