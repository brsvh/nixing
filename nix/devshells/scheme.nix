{
  pkgs,
  ...
}:
{
  devshell = {
    name = "scheme";

    packages = with pkgs; [
      chez
      git
      guile
    ];
  };
}
