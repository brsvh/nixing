{ cell, ... }:
{
  imports = [
    cell.nixosProfiles.english
    cell.nixosProfiles.firewall
    cell.nixosProfiles.network-manager
    cell.nixosProfiles.plymouth
    cell.nixosProfiles.systemd-boot
    cell.nixosProfiles.touchpad
    cell.nixosProfiles.zram
    cell.nixosSuites.base
  ];
}
