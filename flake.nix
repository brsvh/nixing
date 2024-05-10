{
  description = ''
    nixing - a place to collect things about Nix/NixOS
  '';

  nixConfig = {
    experimental-features = [
      "flakes"
      "nix-command"
    ];
  };

  # Nix flake tools
  inputs = {
    flake-compat = {
      url = "github:edolstra/flake-compat/master";
      flake = false;
    };
  };

  outputs = _: { };
}
