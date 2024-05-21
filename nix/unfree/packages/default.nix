{ cell, inputs }:
let
  inherit (inputs) lib nixpkgs;

  pkgs = nixpkgs.appendOverlays [ cell.overlays.unfree ];
in
{
  wemeet = pkgs.wemeet;
}
