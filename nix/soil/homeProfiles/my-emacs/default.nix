{ inputs, ... }:
let
  projectRoot = inputs.self + "/.";
in
{
  programs = {
    my-emacs = {
      defaultEditor = true;
      enable = true;
    };
  };

  xdg = {
    configFile = {
      "my-emacs/authinfo" = {
        source = projectRoot + "/etc/authinfo/authinfo";
      };

      "my-emacs/authinfo.gpg" = {
        source = projectRoot + "/etc/authinfo/authinfo.gpg";
      };
    };
  };
}
