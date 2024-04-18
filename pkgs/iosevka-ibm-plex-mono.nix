{ iosevka, ... }:
iosevka.override {
  set = "iosevka-ibm-plex-mono";

  privateBuildPlan = ''
    [buildPlans.iosevka-ibm-plex-mono]
    family = "Iosevka IBM Plex Mono"
    spacing = "fontconfig-mono"
    serifs = "sans"
    noCvSs = true
    exportGlyphNames = true
    noLigation = true

    [buildPlans.iosevka-ibm-plex-mono.variants]
    inherits = "g"

    [buildPlans.iosevka-ibm-plex-mono.variantswidths.Condensed]
    shape = 500
    menu = 3
    css = "condensed"

    [buildPlans.iosevka-ibm-plex-mono.variants.widths.Normal]
    shape = 600
    menu = 5
    css = "normal"
  '';
}
