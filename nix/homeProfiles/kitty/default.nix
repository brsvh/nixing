{
  config,
  lib,
  ...
}:
let
  inherit (lib)
    mkDefault
    ;
in
{
  programs = {
    kitty = {
      enable = true;

      font =
        let
          cfg = config.fonts.fontconfig.languages.english;
        in
        {
          name = cfg.monospace;
          size = config.fonts.size;
        };

      shellIntegration = {
        enableBashIntegration = mkDefault config.programs.bash.enable;
        enableFishIntegration = mkDefault config.programs.fish.enable;
        enableZshIntegration = mkDefault config.programs.zsh.enable;
        mode = "no-cursor";
      };

      themeFile = "Modus_Vivendi_Tinted";
    };
  };
}
