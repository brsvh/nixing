{ cell, inputs }:
let
  inherit (inputs) emacs-overlay lib nixpkgs;

  projectRoot = inputs.self + "/.";

  my-emacs =
    final: prev:
    let
      prev' = prev.appendOverlays [ emacs-overlay.overlays.default ];

      mkMyEmacsScope = prev'.callPackage ./my-emacs/package.nix { inherit projectRoot; };
    in
    rec {
      my-emacs = my-emacs-master;

      my-emacs-master = lib.makeOverridable mkMyEmacsScope { branch = "master"; };

      my-emacs-stable = lib.makeOverridable mkMyEmacsScope { branch = null; };

      my-emacs-unstable = lib.makeOverridable mkMyEmacsScope { branch = "unstable"; };
    };
in
{
  inherit my-emacs;

  emacs = final: prev: (emacs-overlay.overlays.default final prev) // (my-emacs final prev);
}
