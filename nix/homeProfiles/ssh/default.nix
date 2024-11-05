{
  programs = {
    ssh = {
      enable = true;

      includes = [
        "local/*"
      ];
    };
  };
}
