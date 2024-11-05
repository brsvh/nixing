{
  activities,
  anzu,
  apheleia,
  auctex,
  benchmark-init,
  citar,
  citar-embark,
  citar-org-roam,
  cl-lib,
  company,
  consult,
  consult-org-roam,
  dash,
  diff-hl,
  dired-git-info,
  diredfl,
  doom-modeline,
  eat,
  edit-indirect,
  editorconfig,
  eglot,
  eglot-booster,
  eldoc,
  eldoc-box,
  embark,
  embark-consult,
  embark-org-roam,
  envrc,
  flymake,
  flymake-eslint,
  form-feed,
  gcmh,
  geiser,
  geiser-guile,
  geiser-mit,
  geiser-racket,
  git-cliff,
  git-modes,
  grip-mode,
  haskell-ts-mode,
  hl-todo,
  htmlize,
  ibuffer-project,
  lib,
  linkFarm,
  lsp-mode,
  lua-mode,
  magit,
  magit-section,
  marginalia,
  markdown-mode,
  mermaid-mode,
  modus-themes,
  mu4e,
  mwim,
  nerd-icons,
  nerd-icons-dired,
  nerd-icons-ibuffer,
  nix-ts-mode,
  ob-mermaid,
  on,
  orderless,
  org,
  org-category-capture,
  org-contrib,
  org-modern,
  org-project-capture,
  org-roam,
  org-roam-bibtex,
  org-side-tree,
  ox-reveal,
  ox-tufte,
  pangu-spacing,
  parinfer-rust-mode,
  pdf-tools,
  popper,
  project,
  rainbow-delimiters,
  rainbow-mode,
  rg,
  setup,
  sideline,
  sideline-flymake,
  sideline-lsp,
  smartparens,
  spacious-padding,
  stdenv,
  svg-lib,
  svg-tag-mode,
  switch-window,
  tablist,
  tabspaces,
  transient,
  tree-sitter-grammars,
  trivialBuild,
  valign,
  vertico,
  web-mode,
  with-editor,
  writeText,
  xref,
  yasnippet,
  yasnippet-snippets,
  ...
}:
let
  inherit (builtins)
    attrValues
    map
    ;

  inherit (lib)
    filterAttrs
    getName
    licenses
    maintainers
    pipe
    removeSuffix
    ;

  src = ../../../../../lisp/my-emacs;

  version = "0.2.0";

  meta = {
    description = "My emacs configurations";
    homepage = "https://github.com/brsvh/shelf";
    license = licenses.gpl3Plus;
    maintainers = with maintainers; [ brsvh ];
  };

  mk = writeText "mk.el" ''
    (defmacro mk-subdirs-expr (path)
      `(setq load-path
             (delete-dups (append '(,path)
                                  ',(let ((default-directory path))
                                      (normal-top-level-add-subdirs-to-load-path))
                                  load-path))))
  '';

  treeSitterGrammarsPath = linkFarm "treesit-grammars" (
    map
      (drv: {
        name = "lib${removeSuffix "-grammar" (getName drv)}${stdenv.targetPlatform.extensions.sharedLibrary}";
        path = "${drv}/parser";
      })
      (
        pipe tree-sitter-grammars [
          (filterAttrs (name: _: name != "recurseForDerivations"))
          attrValues
        ]
      )
  );
in
trivialBuild rec {
  inherit
    meta
    src
    version
    ;

  pname = "my-emacs";

  buildInputs = propagatedUserEnvPkgs;

  propagatedUserEnvPkgs = [
    activities
    anzu
    apheleia
    auctex
    benchmark-init
    citar
    citar-embark
    citar-org-roam
    cl-lib
    company
    consult
    consult-org-roam
    dash
    diff-hl
    dired-git-info
    diredfl
    doom-modeline
    eat
    edit-indirect
    editorconfig
    eglot
    eglot-booster
    eldoc
    eldoc-box
    embark
    embark-consult
    embark-org-roam
    envrc
    flymake
    flymake-eslint
    form-feed
    gcmh
    geiser
    geiser-guile
    geiser-mit
    geiser-racket
    git-cliff
    git-modes
    grip-mode
    haskell-ts-mode
    hl-todo
    htmlize
    ibuffer-project
    lsp-mode
    lua-mode
    magit
    magit-section
    marginalia
    markdown-mode
    mermaid-mode
    modus-themes
    mu4e
    mwim
    nerd-icons
    nerd-icons-dired
    nerd-icons-ibuffer
    nix-ts-mode
    ob-mermaid
    on
    orderless
    org
    org-category-capture
    org-contrib
    org-modern
    org-project-capture
    org-roam
    org-roam-bibtex
    org-side-tree
    ox-reveal
    ox-tufte
    parinfer-rust-mode
    pangu-spacing
    pdf-tools
    popper
    project
    rainbow-delimiters
    rainbow-mode
    rg
    setup
    sideline
    sideline-flymake
    sideline-lsp
    smartparens
    spacious-padding
    svg-lib
    svg-tag-mode
    switch-window
    tablist
    tabspaces
    transient
    valign
    vertico
    web-mode
    with-editor
    xref
    yasnippet
    yasnippet-snippets
  ];

  preBuild = ''
    HOME=$(mktemp -d)
  '';

  buildPhase = ''
    runHook preBuild

    emacs -l package -l "${mk}" -f package-initialize --eval "(mk-subdirs-expr \"$PWD\")(add-to-list 'treesit-extra-load-path \"${treeSitterGrammarsPath}\")" -L . --batch -f batch-byte-compile **/*.el

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    LISPDIR=$out/share/emacs/site-lisp
    mkdir -p $LISPDIR
    cp -r * $LISPDIR

    runHook postInstall
  '';
}
