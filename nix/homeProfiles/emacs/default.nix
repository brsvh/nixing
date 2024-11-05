{
  lib,
  my,
  pkgs,
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
    pipe
    removeSuffix
    ;

  inherit (pkgs)
    linkFarm
    stdenv
    tree-sitter-grammars
    ;

  parinferRustEmacsPath = "${pkgs.parinfer-rust-emacs}/lib";

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
{
  imports = [
    my.homeModules.emacs
    my.homeProfiles.direnv
    my.homeProfiles.email
    my.homeProfiles.git
    my.homeProfiles.texlive
    my.homeProfiles.tools
  ];

  programs = {
    emacs = {
      enable = true;

      extraDependencies =
        with pkgs;
        [
          editorconfig-checker
          emacs-lsp-booster
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
          rust-analyzer
          rustc
          rustfmt
          stylua
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
        ++ (with rubyPackages; [
          sass
        ]);

      extraEarlyInitConfig = ''
        ;; Avoid garbage collection during the initialization to achieve a
        ;; faster startup.
        (setq gc-cons-threshold most-positive-fixnum)

        ;; Prevent check mtime of Emacs Lisp Bytecode file to save time.
        (setq load-prefer-newer noninteractive)

        ;; Play the prelude of my-emacs.
        (require 'my-prelude)
      '';

      extraEarlyInitHeader = ''
        ;; Copyright (C) 2022-2024 Burgess Chang

        ;; Author: Burgess Chang <bsc@brsvh.org>
        ;; Keywords: local
        ;; Package-Requires: ((emacs "29.1"))
        ;; Version: 0.1.0
      '';

      extraInitConfig = ''
        (require 'my-prelude)
        (require 'my-lib)
        (require 'my-core)

        

        (require 'my-comint)
        (require 'my-dired)
        (require 'my-editor)
        (require 'my-email)
        (require 'my-mule)
        (require 'my-project)
        (require 'my-security)
        (require 'my-terminal)
        (require 'my-ui)
        (require 'my-workflow)
        (require 'my-workspace)

        

        (setup mermaid-mode
          (:set
           mermaid-mmdc-location "${pkgs.mermaid-cli}/bin/mmdc"))

        (setup ob-mermaid
          (:set
           ob-mermaid-cli-path "${pkgs.mermaid-cli}/bin/mmdc"))

        (setup parinfer-rust
          (:set
           parinfer-rust-auto-download nil
           parinfer-rust-library "${parinferRustEmacsPath}/libparinfer_rust.so"
           parinfer-rust-library-directory "${parinferRustEmacsPath}/lib/"))

        (setup treesit-grammars
          (:snoc
           treesit-extra-load-path "${treeSitterGrammarsPath}"))

        

        (require 'my-drawing)
        (require 'my-programming)
        (require 'my-writing)

        

        (require 'my-postlude)
      '';

      extraInitHeader = ''
        ;; Copyright (C) 2022-2024 Burgess Chang

        ;; Author: Burgess Chang <bsc@brsvh.org>
        ;; Keywords: local
        ;; Package-Requires: ((emacs "29.1"))
        ;; Version: 0.1.0
      '';

      extraPackages =
        epkgs: with epkgs; [
          my-emacs
        ];

      package = pkgs.emacs30-gtk3;

      isDefaultEditor = true;
    };
  };
}
