{
  config,
  my,
  pkgs,
  ...
}:
{
  imports = [
    my.homeProfiles.envvars
  ];

  home = {
    packages = with pkgs; [
      ghostscript
    ];

    sessionVariables = {
      TEXMFHOME = "${config.xdg.dataHome}/texmf";
      TEXMFVAR = "${config.xdg.cacheHome}/texlive/texmf-var";
      TEXMFCONFIG = "${config.xdg.configHome}/texlive/texmf-config";
    };
  };

  programs = {
    texlive = {
      enable = true;
      extraPackages = tpkgs: {
        inherit (tpkgs)
          scheme-full
          ;
      };
    };
  };
}
