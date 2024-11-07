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
  dired-open,
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
  mu4e-alert,
  mu4e-marker-icons,
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

  funcs = writeText "funcs.sh" ''
    addToEmacsLoadPath() {
      local lispDir="$1"
      if [[ -d $lispDir && ''\${EMACSLOADPATH-} != *"$lispDir":* ]] ; then
        # It turns out, that the trailing : is actually required
        # see https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Search.html
        export EMACSLOADPATH="$lispDir:''\${EMACSLOADPATH-}"
      fi
    }

    addToEmacsNativeLoadPath() {
      local nativeDir="$1"
      if [[ -d $nativeDir && ''\${EMACSNATIVELOADPATH-} != *"$nativeDir":* ]]; then
        export EMACSNATIVELOADPATH="$nativeDir:''\${EMACSNATIVELOADPATH-}"
      fi
    }

    addEmacsVars () {
      addToEmacsLoadPath "$1/share/emacs/site-lisp"

      if [ -n "''\${addEmacsNativeLoadPath:-}" ]; then
        addToEmacsNativeLoadPath "$1/share/emacs/native-lisp"
      fi
    }
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
    dired-open
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
    mu4e-alert
    mu4e-marker-icons
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

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/emacs/site-lisp
    cp -r $src $out/share/emacs/site-lisp/my-emacs
    chmod -R u+w $out/share/emacs/site-lisp/my-emacs

    find $out/share/emacs -type f -name "*.el" -not -name ".dir-locals.el" -print0 \
      | xargs --verbose -0 -I {} -n 1 -P $NIX_BUILD_CORES sh -c \
        "emacs \
          --batch \
          -l package \
          -l \"${mk}\" \
          -f package-initialize \
          --eval \"(mk-subdirs-expr \\\"$PWD\\\")\" \
          -L . \
          --eval \"(add-to-list 'treesit-extra-load-path \\\"${treeSitterGrammarsPath}\\\")\" \
          -f batch-byte-compile {}"

    # runHook postInstall
    # See https://github.com/NixOS/nixpkgs/blob/a842d95951d8aecbfb9350f7e5e9b68b473e796c/pkgs/applications/editors/emacs/build-support/generic.nix#L82-L102
    mkdir -p $out/share/emacs/native-lisp
    source ${funcs}
    addEmacsVars "$out"

    find $out/share/emacs -type f -name "*.el" -not -name ".dir-locals.el" -print0 \
      | xargs --verbose -0 -I {} -n 1 -P $NIX_BUILD_CORES sh -c \
          "emacs \
             --batch \
             -f package-activate-all \
             --eval \"(setq native-comp-eln-load-path (cdr native-comp-eln-load-path))\" \
             --eval \"(let ((default-directory \\\"$out/share/emacs/site-lisp\\\")) (normal-top-level-add-subdirs-to-load-path))\" \
             --eval \"(setq large-file-warning-threshold nil)\" \
             --eval \"(setq byte-compile-error-on-warn nil)\" \
             --eval \"(add-to-list 'treesit-extra-load-path \\\"${treeSitterGrammarsPath}\\\")\" \
             -f batch-native-compile {}"
  '';
}
