{ iosevka, ... }:
iosevka.override {
  set = "source-code-pro";

  privateBuildPlan = ''
    [buildPlans.iosevka-source-code-pro]
    family = "Iosevka Source Code Pro"
    spacing = "fontconfig-mono"
    serifs = "sans"
    noCvSs = true
    exportGlyphNames = true
    noLigation = true

    [buildPlans.iosevka-source-code-pro.variants]
    inherits = "ss09"

    [buildPlans.iosevka-source-code-pro.variantswidths.Condensed]
    shape = 500
    menu = 3
    css = "condensed"

    [buildPlans.iosevka-source-code-pro.variants.widths.Normal]
    shape = 600
    menu = 5
    css = "normal"
  '';
}
