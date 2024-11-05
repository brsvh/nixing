{
  pkgs,
  ...
}:
{
  devshell = {
    name = "c/c++";

    packages = with pkgs; [
      git
      clang-tools
      clang
    ];
  };
}
