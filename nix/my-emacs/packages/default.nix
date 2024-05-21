{ cell, inputs }:
let
  inherit (inputs) emacs-overlay lib nixpkgs;

  projectRoot = inputs.self + "/.";

  callPackage =
    pkg: lib.callPackageWith (nixpkgs.appendOverlays [ emacs-overlay.overlays.default ]) pkg;

  mkMyEmacsScope = callPackage ./my-emacs/package.nix { inherit projectRoot; };
in
rec {
  my-emacs = my-emacs-master;

  my-emacs-master = lib.makeOverridable mkMyEmacsScope { branch = "master"; };

  my-emacs-stable = my-emacs-master.override { branch = null; };

  my-emacs-unstable = my-emacs-master.override { branch = "unstable"; };
}
