{
  inputs,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    mkForce
    ;

  inherit (inputs)
    lanzaboote
    ;
in
{
  imports = [
    lanzaboote.nixosModules.lanzaboote
  ];

  boot = {
    lanzaboote = {
      enable = true;
      pkiBundle = "/etc/secureboot";
    };

    loader = {
      systemd-boot = {
        enable = mkForce false;
      };
    };
  };

  environment = {
    systemPackages = with pkgs; [
      sbctl
    ];
  };
}
