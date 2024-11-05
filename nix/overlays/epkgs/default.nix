final: prev:
let
  ignoreCompilationError =
    pkg:
    pkg.overrideAttrs (
      finalAttrs: previousAttrs: {
        ignoreCompilationError = true;
      }
    );
in
{
  emacsPackagesFor =
    emacs:
    let
      inherit (prev)
        emacsPackagesFor
        ;

      inherit (prev.lib)
        packagesFromDirectoryRecursive
        ;

      scope = emacsPackagesFor emacs;

      epkgs =
        finalEpkgs: prevEpkgs:
        prevEpkgs.override {
          elpaPackages = prevEpkgs.elpaPackages // {
            setup = ignoreCompilationError prevEpkgs.elpaPackages.setup;
          };

          manualPackages =
            prevEpkgs.manualPackages
            // packagesFromDirectoryRecursive {
              inherit (finalEpkgs)
                callPackage
                ;

              directory = ../../packages/emacs/manualPackages;
            };
        };
    in
    scope.overrideScope epkgs;
}
