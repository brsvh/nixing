{ cell, ... }:
{
  imports = [
    cell.nixosProfiles.bluetooth
    cell.nixosProfiles.touchpad
  ];
}
