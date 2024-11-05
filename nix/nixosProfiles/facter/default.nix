{
  inputs,
  ...
}:
let
  inherit (inputs)
    facter
    ;
in
{
  imports = [
    facter.nixosModules.facter
  ];
}
