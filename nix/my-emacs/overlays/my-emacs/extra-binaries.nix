pkgs:
with pkgs;
[
  direnv
  editorconfig-checker
  emacs-lsp-booster
  git-cliff
  graphviz
  guile
  mailutils
  mitscheme
  multimarkdown
  nil
  nixfmt-rfc-style
  racket-minimal
  ripgrep
  texliveFull
  vscode-langservers-extracted
]
++ (with llvmPackages; [ clang-tools ])
++ (with nodePackages; [
  prettier
  typescript-language-server
])
++ (with python3Packages; [ grip ])
