{ cell, inputs }:
let
  inherit (inputs) haumea hive lib;

  load =
    {
      inputs,
      cell,
      src,
    }:
    haumea.lib.load {
      inherit src;

      inputs = {
        inherit cell inputs;
      };
      transformer = haumea.lib.transformers.liftDefault;
    };

  findLoad =
    {
      inputs,
      cell,
      block,
    }:
    lib.mapAttrs' (
      n: _:
      lib.nameValuePair (lib.removeSuffix ".nix" n) (load {
        inherit inputs cell;
        src = block + /${n};
      })
    ) (lib.removeAttrs (lib.readDir block) [ "default.nix" ]);
in
findLoad {
  inherit cell inputs;

  block = ./.;
}
