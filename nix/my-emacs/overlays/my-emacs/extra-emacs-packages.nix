# NOTE: All packages related to magit should use the versions available
#       in MELPA.
epkgs:
with epkgs;
(with elpaPackages; [
  activities
  auctex
  cl-lib
  compat
  consult
  dash
  diff-hl
  dired-git-info
  embark
  embark-consult
  gcmh
  let-alist
  marginalia
  orderless
  org
  org-modern
  persist
  popper
  project
  queue
  rainbow-mode
  seq
  setup
  svg-lib
  transient
  vertico
  yasnippet
])
++ (with manualPackages; [ my.on ])
++ (with melpaPackages; [
  aio
  apheleia
  benchmark-init
  biblio
  biblio-core
  bibtex-completion
  citar
  citar-embark
  citar-org-roam
  citeproc
  company
  consult-org-roam
  diredfl
  docker
  doom-modeline
  emacsql
  embark-org-roam
  envrc
  f
  frameshot
  git-cliff
  git-commit
  git-modes
  grip-mode
  hl-todo
  ibuffer-project
  inheritenv
  magit
  magit-section
  modus-themes
  mwim
  nerd-icons
  nerd-icons-dired
  nerd-icons-ibuffer
  nix-ts-mode
  org-category-capture
  org-project-capture
  org-roam
  org-roam-bibtex
  org-side-tree
  page-break-lines
  pangu-spacing
  parsebib
  pdf-tools
  rainbow-delimiters
  rg
  s
  shrink-path
  smartparens
  string-inflection
  svg-tag-mode
  switch-window
  tabspaces
  valign
])
++ (with nongnuPackages; [
  anzu
  edit-indirect
  editorconfig
  hl-block-mode
  markdown-mode
  org-contrib
  tablist
  with-editor
  wgrep
  yasnippet-snippets
])
