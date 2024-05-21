# Copyright (C) 2022-2024 Burgess Chang

# This file is part of my-emacs.

# my-emacs is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation, either version 3 of the License, or (at your
# option) any later version.

# my-emacs is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with my-emacs.  If not, see <https://www.gnu.org/licenses/>.

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
  doom-modeline
  emacsql
  embark-org-roam
  f
  frameshot
  git-cliff
  git-commit
  git-modes
  grip-mode
  hl-todo
  ibuffer-project
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
