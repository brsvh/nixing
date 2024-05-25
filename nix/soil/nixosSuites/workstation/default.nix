{ cell, inputs, ... }:
let
  inherit (inputs) lanzaboote;
  inherit (inputs.cells) fonts my-emacs;
in
{
  imports = [
    cell.nixosProfiles.alsa
    cell.nixosProfiles.chinese
    cell.nixosProfiles.english
    cell.nixosProfiles.firewall
    cell.nixosProfiles.fonts
    cell.nixosProfiles.japanese
    cell.nixosProfiles.korean
    cell.nixosProfiles.network-manager
    cell.nixosProfiles.pipewire
    cell.nixosProfiles.plymouth
    cell.nixosProfiles.rtkit
    cell.nixosProfiles.starship
    cell.nixosProfiles.systemd-boot
    cell.nixosProfiles.zram
    cell.nixosSuites.base
    fonts.nixosModules.fonts
    fonts.nixosModules.tsangertype-fonts
    # FIXME this module will cause infinite recursion.
    # lanzaboote.nixosModules.lanzaboote;
    my-emacs.nixosModules.my-emacs
  ];
}
