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
  vscode-langservers-extracted
]
++ (with llvmPackages; [ clang-tools ])
++ (with nodePackages; [
  prettier
  typescript-language-server
])
++ (with python3Packages; [ grip ])
