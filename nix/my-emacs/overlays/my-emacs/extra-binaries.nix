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
++ (with haskellPackages; [
  ghc
  haskell-language-server
])
++ (with llvmPackages; [ clang-tools ])
++ (with nodePackages; [
  prettier
  typescript-language-server
])
++ (with python3Packages; [ grip ])
