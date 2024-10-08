{
  cell,
  pkgs,
  ...
}:
let
  fullname = "Bingshan Chang";
  username = "changbingshan";
in
{
  imports = [ cell.nixosProfiles.fish ];

  home-manager = {
    backupFileExtension = "backup";

    users = {
      "${username}" =
        { config, ... }:
        {
          imports = [
            cell.homeUsers.changbingshan
          ];
        };
    };
  };

  users = {
    mutableUsers = true;

    users = {
      "${username}" = {
        description = fullname;

        extraGroups = [
          "audio"
          "docker"
          "jackaudio"
          "libvirtd"
          "wheel"
          "networkmanager"
        ];

        initialHashedPassword = "$6$cB3EK3Lynl./0Bio$bgH7P93D1lpgvEIJ3iks7Dk2IyNue7ria2aH8.xkZZ1PPooxkb7p/bEMN1UtJaV0TIeVr/eTY8oAN38vBgMKe0";
        isNormalUser = true;

        openssh = {
          authorizedKeys = {
            keys = [
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF8kaNm6jre8kIxo3LSWe9Ohz8tXc4jRcSCw1BeoC7tj bsc@eustoma"
            ];
          };
        };

        shell = pkgs.fish;
      };
    };
  };
}
