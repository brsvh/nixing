{ inputs
, ...
}:
{
  imports = [
    inputs.treefmt.flakeModule
  ];
  perSystem = { ... }: {
    treefmt = {
      flakeFormatter = true;
      projectRootFile = "flake.nix";
      programs = {
        nixpkgs-fmt = {
          enable = true;
        };
      };
    };
  };
}
