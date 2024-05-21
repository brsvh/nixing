{ cell, inputs }:
let
  inherit (inputs) emacs-overlay lib nixpkgs;

  projectRoot = inputs.self + "/.";

  callPackage =
    pkg: lib.callPackageWith (nixpkgs.appendOverlays [ emacs-overlay.overlays.default ]) pkg;

  mkMyEmacsScope = callPackage ./my-emacs/package.nix { inherit projectRoot; };
in
{
  my-emacs =
    final: prev:
    (emacs-overlay.overlays.default final prev)
    // rec {
      my-emacs = my-emacs-master;

      my-emacs-master = lib.makeOverridable mkMyEmacsScope { branch = "master"; };

      my-emacs-stable = lib.makeOverridable mkMyEmacsScope { branch = null; };

      my-emacs-unstable = lib.makeOverridable mkMyEmacsScope { branch = "unstable"; };
    };
}
