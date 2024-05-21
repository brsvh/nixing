pkgs:
with pkgs;
[
  conform
  editorconfig-checker
  git-cliff
  graphviz
  lefthook
  mailutils
  multimarkdown
  nixfmt-rfc-style
  ripgrep
  texliveFull
  treefmt
]
++ (with llvmPackages; [
  bintools
  clangUseLLVM
])
++ (with python3Packages; [ grip ])
