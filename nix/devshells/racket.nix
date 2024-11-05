{
  pkgs,
  ...
}:
{
  devshell = {
    name = "racket";

    packages = with pkgs; [
      git
      racket-minimal
      zuo
    ];
  };
}
