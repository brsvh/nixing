{
  home = {
    packages = with pkgs; [ microsoft-edge ];
  };

  programs = {
    google-chrome = {
      enable = true;
    };
  };
}
