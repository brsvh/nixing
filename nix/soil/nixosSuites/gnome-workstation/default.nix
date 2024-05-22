{ cell, ... }:
{
  imports = [
    cell.nixosProfiles.gnome
    cell.nixosSuites.workstation
  ];
}
