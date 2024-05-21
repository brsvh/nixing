{ cell, inputs }:
let
  inherit (inputs) lib nixpkgs;

  pkgs = nixpkgs.appendOverlays [ cell.overlays.my-emacs ];
in
{
  my-emacs-nogui = pkgs.my-emacs.nogui;

  my-emacs-pgtk = pkgs.my-emacs.default;

  my-emacs-tools = pkgs.my-emacs.instruments;

  my-emacs-x11 = pkgs.my-emacs.x11;
}
