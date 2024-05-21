{ cell, ... }:
{
  imports = [
    cell.nixosProfiles.alsa
    cell.nixosProfiles.bluetooth
    cell.nixosProfiles.english
    cell.nixosProfiles.firewall
    cell.nixosProfiles.network-manager
    cell.nixosProfiles.pipewire
    cell.nixosProfiles.plymouth
    cell.nixosProfiles.rtkit
    cell.nixosProfiles.systemd-boot
    cell.nixosProfiles.touchpad
    cell.nixosProfiles.zram
    cell.nixosSuites.base
  ];
}
