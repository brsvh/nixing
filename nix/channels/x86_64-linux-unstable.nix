{
  nixpkgs-unstable,
  overlays,
  x86_64-linux,
  ...
}:
import nixpkgs-unstable {
  inherit
    overlays
    ;

  config = {
    allowUnfree = true;
  };

  system = x86_64-linux;
}
