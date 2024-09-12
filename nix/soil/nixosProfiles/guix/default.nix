{
  services = {
    guix = {
      enable = true;

      gc = {
        dates = "monthly";
        enable = true;

        extraArgs = [
          "--delete-generations"
          "--optimize"
        ];
      };
    };
  };
}
