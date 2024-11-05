{
  programs = {
    bash = {
      interactiveShellInit = ''
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
    };
  };
}
