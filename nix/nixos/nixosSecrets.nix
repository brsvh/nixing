{ cell, inputs }:
let
  inherit (inputs.hive) findLoad;
in
findLoad {
  inherit cell inputs;
  block = ./secrets;
}
