{
  inputs,
  ...
}:
let
  inherit (inputs)
    disko
    ;
in
{
  imports = [
    disko.nixosModules.disko
  ];
}
