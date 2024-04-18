{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.workstation.shell;

  desktop = config.workstation.desktop;

  profileText = '''';

  rcText = ''
    case :$SHELLOPTS: in
      *:posix:*)
        _bash_type="sh"
        ;;
      *)
        _bash_type="bash"
        ;;
    esac

    export HISTFILE=''${XDG_CACHE_HOME:-''$HOME/.cache}/''${_bash_type}/history
    mkdir -p $(dirname $HISTFILE)
  '';
in
{
  config = {
    programs = {
      bash = {
        interactiveShellInit = rcText;
        loginShellInit = profileText;
      };
    };
  };
}
