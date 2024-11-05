# REVIEW remove this file after nixos-facter support setup Touch Pad
# See https://github.com/numtide/nixos-facter-modules/issues/47 .
{
  services = {
    libinput = {
      enable = true;
    };
  };
}
