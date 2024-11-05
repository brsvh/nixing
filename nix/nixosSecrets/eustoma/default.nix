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
    sops.nixosModules.sops
  ];

  services = {
    dae = {
      configFile = config.sops.secrets."dae/config.dae".path;
    };
  };

  sops = {
    age = {
      keyFile = "/var/lib/sops/key.txt";
      generateKey = true;
      sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
    };

    defaultSopsFile = my.etc.sops.eustoma;

    secrets = {
      "dae/config.dae" = {
        restartUnits = [
          "dae.service"
        ];
      };

      "tokens/nixAccessTokens.conf" = {
        mode = "0440";
        group = "users";
      };
    };
  };
}
