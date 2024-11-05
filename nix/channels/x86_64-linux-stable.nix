{
  nixpkgs-stable,
  overlays,
  x86_64-linux,
  ...
}:
import nixpkgs-stable {
  inherit
    overlays
    ;

  config = {
    allowUnfree = true;
  };

  system = x86_64-linux;
}
