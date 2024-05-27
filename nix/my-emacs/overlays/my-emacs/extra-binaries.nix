pkgs:
with pkgs;
[
  conform
  direnv
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
++ (with nodePackages; [ prettier ])
++ (with python3Packages; [ grip ])
