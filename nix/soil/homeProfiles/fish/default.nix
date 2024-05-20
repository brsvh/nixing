{
  programs = {
    fish = {
      enable = true;
      interactiveShellInit = ''
        set fish_greeting
      '';
    };

    fzf = {
      enable = true;
      enableFishIntegration = true;
    };
  };
}
