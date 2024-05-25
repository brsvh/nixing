{ lib, pkgs, ... }:
{
  boot = {
    lanzaboote = {
      enable = true;
      pkiBundle = "/etc/secureboot";
    };

    loader = {
      systemd-boot = {
        enable = lib.mkForce true;
      };
    };
  };

  environment = {
    systemPackages = with pkgs; [ sbctl ];
  };
}
