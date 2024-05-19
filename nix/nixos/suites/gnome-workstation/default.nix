{ cell, ... }:
{
  imports = [
    cell.nixosProfiles.firewall
    cell.nixosProfiles.gnome
    cell.nixosProfiles.network-manager
  ];
}
