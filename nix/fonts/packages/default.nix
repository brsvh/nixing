{ cell, inputs }:
let
  inherit (inputs) lib nixpkgs;

  pkgs = nixpkgs.appendOverlays [ cell.overlays.nonfree-fonts ];
in
{
  foundertype-fonts = pkgs.foundertype-fonts;
}
