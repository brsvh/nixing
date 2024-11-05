{
  pkgs,
  ...
}:
{
  home = {
    packages = with pkgs; [
      rnote
    ];
  };
}
