pkgs:
with pkgs;
[
  direnv
  editorconfig-checker
  emacs-lsp-booster
  git-cliff
  graphviz
  guile
  lua-language-server
  mailutils
  mermaid-cli
  mitscheme
  multimarkdown
  nil
  nix
  nixfmt-rfc-style
  pyright
  racket-minimal
  ripgrep
  rust-analyzer
  rustc
  rustfmt
  stylua
  texliveFull
  vscode-langservers-extracted
]
++ (with haskellPackages; [
  ghc
  haskell-language-server
])
++ (with llvmPackages; [
  clang
  clang-tools
])
++ (with nodePackages; [
  eslint
  prettier
  typescript
  typescript-language-server
])
++ (with python3Packages; [
  black
  grip
  python
])
++ (with rubyPackages; [ sass ])
