{
  callPackage,
  emacs,
  emacs-git,
  emacs-git-nox,
  emacs-gtk,
  emacs-nox,
  emacs-pgtk,
  emacs-unstable,
  emacs-unstable-nox,
  emacs-unstable-pgtk,
  emacsPackagesFor,
  lib,
  newScope,
  pkgs,
  parinfer-rust-emacs,
  projectRoot,
  runCommand,
  source-code-pro,
  source-sans-pro,
  source-serif-pro,
  tree-sitter-grammars,
  xorg,
}:
with lib;
{
  branch ? "master",
  extraBinaries ? [ ],
  extraConfig ? "",
  extraEmacsPackages ? (epkgs: [ ]),
  extraFonts ? [ ],
  extraLibraries ? [ ],
}:
let
  mkMyEmacs = callPackage ./make-my-emacs.nix { };

  mkMyEmacsInitDirectory = callPackage ./make-my-emacs-init-directory.nix { inherit projectRoot; };

  dependencies =
    epkgs:
    (with epkgs; [
      elpaPackages.setup
      melpaPackages.parinfer-rust-mode
    ])
    ++ ((import ./extra-emacs-packages.nix) epkgs)
    ++ (extraEmacsPackages epkgs);

  binaries =
    with pkgs;
    [
      fd
      ripgrep
    ]
    ++ (import ./extra-binaries.nix pkgs)
    ++ extraBinaries;

  fonts = [
    source-code-pro
    source-sans-pro
    source-serif-pro
  ] ++ (import ./extra-fonts.nix pkgs) ++ extraFonts;

  libraries =
    [ parinfer-rust-emacs ]
    ++ (pipe tree-sitter-grammars [
      (filterAttrs (name: _: name != "recurseForDerivations"))
      attrValues
    ])
    ++ (import ./extra-libraries.nix pkgs);

  binSymlinkJoin =
    args_@{
      name,
      paths,
      preferLocalBuild ? true,
      allowSubstitutes ? false,
      postBuild ? "",
      ...
    }:
    let
      args =
        removeAttrs args_ [
          "name"
          "postBuild"
        ]
        // {
          inherit preferLocalBuild allowSubstitutes;
          passAsFile = [ "paths" ];
        };
    in
    runCommand name args ''
      mkdir -p $out/bin
      for i in $(cat $pathsPath); do
        ${xorg.lndir}/bin/lndir -silent $i/bin $out/bin
      done
      ${postBuild}
    '';

  instruments = binSymlinkJoin {
    name = "my-emacs-instruments";
    paths = binaries;
  };

  emacsPackagesFor' =
    drv:
    (emacsPackagesFor drv).overrideScope (
      finalEpkgs: prevEpkgs:
      let
        manualPackages = prevEpkgs.manualPackages // {
          my = callPackage ./manual-packages {
            inherit prevEpkgs;
            inherit (prevEpkgs) trivialBuild;
            emacs = drv;
          };
        };
      in
      prevEpkgs.override { inherit manualPackages; }
    );

  getPlainEmacs = drv: (emacsPackagesFor' drv).emacsWithPackages dependencies;

  default =
    let
      vanillaEmacs =
        if branch == "master" then
          emacs-pgtk
        else if branch == "unstable" then
          emacs-unstable-pgtk
        else
          emacs-gtk;

      plainEmacs = (getPlainEmacs vanillaEmacs);

      initDirectory = mkMyEmacsInitDirectory plainEmacs extraConfig;
    in
    mkMyEmacs {
      inherit
        binaries
        fonts
        initDirectory
        libraries
        plainEmacs
        vanillaEmacs
        ;
    };

  nogui =
    let
      vanillaEmacs =
        if branch == "master" then
          emacs-git-nox
        else if branch == "unstable" then
          emacs-unstable-nox
        else
          emacs-nox;

      plainEmacs = (getPlainEmacs vanillaEmacs);

      initDirectory = mkMyEmacsInitDirectory plainEmacs extraConfig;
    in
    mkMyEmacs {
      inherit
        binaries
        fonts
        initDirectory
        libraries
        plainEmacs
        vanillaEmacs
        ;
    };

  x11 =
    let
      vanillaEmacs =
        if branch == "master" then
          emacs-git
        else if branch == "unstable" then
          emacs-unstable
        else
          emacs;

      plainEmacs = (getPlainEmacs vanillaEmacs);

      initDirectory = mkMyEmacsInitDirectory plainEmacs extraConfig;
    in
    mkMyEmacs {
      inherit
        binaries
        fonts
        initDirectory
        libraries
        plainEmacs
        vanillaEmacs
        ;
    };
in
makeScope newScope (self: {
  inherit
    binaries
    default
    fonts
    instruments
    libraries
    nogui
    x11
    ;
})
