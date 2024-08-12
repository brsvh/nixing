{ cell, ... }:
{
  imports = [
    cell.nixosProfiles.plasma
    cell.nixosSuites.workstation
  ];
}
