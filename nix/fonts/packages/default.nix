{ cell, inputs }:
let
  inherit (inputs) lib nixpkgs;

  pkgs = nixpkgs.appendOverlays [ cell.overlays.proprius-fonts ];
in
{
  foundertype-fonts = pkgs.foundertype-fonts;

  tsangertype-fonts = pkgs.tsangertypeFonts.gratisProPersona;

  tsangertype-gpc-fonts = pkgs.tsangertypeFonts.gratisProCommercium;
}
