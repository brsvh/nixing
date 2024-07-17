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
  mermaid-cli
  mitscheme
  multimarkdown
  nil
  nixfmt-rfc-style
  pyright
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
  eslint
  prettier
  typescript-language-server
])
++ (with python3Packages; [
  black
  grip
  python
])
