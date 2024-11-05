{
  inputs,
  self,
  ...
}:
let
  inherit (inputs)
    browser
    emacs-overlay
    nix-alien
    nixpkgs
    rust-overlay
    ;
in
{
  nix = {
    registry = {
      nixpkgs = {
        flake = nixpkgs;
      };
    };
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
    };

    overlays = [
      nix-alien.overlays.default
      self.overlays.epkgs
      self.overlays.fonts
      (
        final: prev:
        let
          inherit (prev.stdenv)
            system
            ;
        in
        if system == "x86_64-linux" then
          {
            inherit (browser.packages.${system})
              google-chrome
              google-chrome-beta
              google-chrome-dev
              ;
          }
        else
          { }
      )
    ];
  };
}
