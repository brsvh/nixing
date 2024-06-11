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
  nil
  nixfmt-rfc-style
  ripgrep
  texliveFull
]
++ (with llvmPackages; [ clang-tools ])
++ (with nodePackages; [ prettier ])
++ (with python3Packages; [ grip ])
