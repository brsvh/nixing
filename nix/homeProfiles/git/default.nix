{
  programs = {
    git = {
      enable = true;

      ignores = [
        "TODO.org"
      ];

      lfs = {
        enable = true;
      };
    };

    git-cliff = {
      enable = true;
    };
  };
}
