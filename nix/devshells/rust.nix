{
  pkgs,
  ...
}:
{

  devshell = {
    name = "rust";

    packages = with pkgs; [
      git
      rust-bin.stable.latest.default
    ];
  };
}
