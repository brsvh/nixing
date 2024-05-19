{ cell, pkgs, ... }:
{
  imports = [ cell.nixosProfiles.fish ];

  users = {
    mutableUsers = true;

    users = {
      changbingshan = {
        description = "Bingshan Chang";

        extraGroups = [
          "audio"
          "jackaudio"
          "libvirtd"
          "wheel"
          "networkmanager"
        ];

        initialHashedPassword = "$6$cB3EK3Lynl./0Bio$bgH7P93D1lpgvEIJ3iks7Dk2IyNue7ria2aH8.xkZZ1PPooxkb7p/bEMN1UtJaV0TIeVr/eTY8oAN38vBgMKe0";
        isNormalUser = true;
        shell = pkgs.fish;
      };
    };
  };
}
