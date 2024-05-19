{ cell, inputs }:
let
  inherit (inputs.haumea.lib) load transformers;
in
load {
  inputs = {
    inherit cell inputs;
  };

  src = ./disko;
  transformer = transformers.liftDefault;
}
