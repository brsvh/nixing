pkgs:
with pkgs;
[
  direnv
  editorconfig-checker
  emacs-lsp-booster
  git-cliff
  graphviz
  mailutils
  multimarkdown
  nixfmt-rfc-style
  ripgrep
  texliveFull
]
++ (with llvmPackages_18; [
  bintools-unwrapped
  clang-unwrapped
  mlir
])
++ (with nodePackages; [ prettier ])
++ (with python3Packages; [ grip ])
