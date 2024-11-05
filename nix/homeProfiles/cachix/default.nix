{
  pkgs,
  ...
}:
{
  home = {
    packages = with pkgs; [
      cachix
    ];
  };
}
