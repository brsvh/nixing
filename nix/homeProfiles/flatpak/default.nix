{
  inputs,
  ...
}:
let
  inherit (builtins)
    toString
    ;

  inherit (inputs)
    nix-flatpak
    ;
in
{
  imports = [
    nix-flatpak.homeManagerModules.nix-flatpak
  ];

  services = {
    flatpak = {
      enable = true;

      overrides = {
        global = {
          Context = {
            filesystems = [
              "/nix/store:ro"
            ];
          };
        };
      };

      packages = [
        "com.github.tchx84.Flatseal"
      ];
    };
  };
}
