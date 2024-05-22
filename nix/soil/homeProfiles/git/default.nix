{
  programs = {
    git = {
      enable = true;

      ignores = [
        # Per-project GTD.
        "TODO.org"
      ];

      lfs = {
        enable = true;
      };
    };
  };
}
