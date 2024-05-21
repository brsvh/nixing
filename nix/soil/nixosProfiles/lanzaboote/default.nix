{
  inputs,
  lib,
  pkgs,
  ...
}:
let
  inherit (inputs) lanzaboote;
in
{
  imports = [ lanzaboote.nixosModules.lanzaboote ];

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
