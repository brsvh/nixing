{
  config,
  inputs,
  my,
  ...
}:
let
  inherit (inputs)
    home-manager
    ;
in
{
  imports = [
    home-manager.nixosModules.home-manager
  ];

  home-manager = {
    backupFileExtension = "hm-backup";

    extraSpecialArgs = {
      inherit
        inputs
        my
        ;

      osConfig = config;
    };

    useGlobalPkgs = true;
    useUserPackages = true;
  };
}
