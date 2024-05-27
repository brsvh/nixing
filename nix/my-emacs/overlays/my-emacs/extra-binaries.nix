pkgs:
with pkgs;
[
  conform
  direnv
  editorconfig-checker
  emacs-lsp-booster
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
++ (with llvmPackages_18; [
  bintools-unwrapped
  clang-unwrapped
  mlir
])
++ (with nodePackages; [ prettier ])
++ (with python3Packages; [ grip ])
