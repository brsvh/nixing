{ cell, inputs }:
let
  inherit (inputs) nixpkgs std;

  lib = nixpkgs.lib // builtins;

  pkgs = nixpkgs;
in
with std.lib;
{
  conform = dev.mkNixago cfg.conform {
    data = {
      commit = {
        body = {
          required = false;
        };

        conventional = {
          types = [
            "build"
            "chore"
            "ci"
            "docs"
            "feat"
            "refactor"
            "style"
            "test"
            "fix"
          ];

          scopes = [
            "ci"
            "flake"
          ];
        };

        descriptionLength = 72;

        gpg = {
          required = true;
        };

        header = {
          length = 68;
          imperative = true;
        };

        maximumOfOneCommit = false;
      };
    };
  };

  editorconfig = dev.mkNixago cfg.editorconfig {
    data = {
      root = true;

      "*" = {
        charset = "utf-8";
        end_of_line = "lf";
        indent_size = 2;
        indent_style = "space";
        insert_final_newline = true;
        max_line_length = 72;
        tab_width = 4;
        trim_trailing_whitespace = true;
      };

      "*.nix" = {
        tab_width = 2;
      };
    };
  };

  lefthook = dev.mkNixago cfg.lefthook {
    data = {
      commit-msg = {
        commands = {
          conform = {
            run = ''
              # Allow wip, fixup, squash.
              [[ "$(head -n 1 {1})" =~ ^WIP(:.*)?$|^wip(:.*)?$|fixup\!.*|squash\!.* ]] ||
              conform enforce --commit-msg-file {1}
            '';

            skip = [
              "merge"
              "rebase"
            ];
          };
        };
      };

      pre-commit = {
        commands = {
          treefmt = {
            run = "treefmt --fail-on-change {staged_files}";
            skip = [
              "merge"
              "rebase"
            ];
          };
        };

        skip = [ { ref = "update_flake_lock_action"; } ];
      };
    };
  };

  mdbook = dev.mkNixago cfg.mdbook {
    data = {
      book = {
        authors = [ "Burgess Chang" ];
        language = "en";
        multilingual = false;
        src = "doc";
        title = "nixing";
      };

      build = {
        build-dir = "result-book";
      };

      output = {
        html = {
          no-section-label = true;
        };
      };
    };
  };

  sops =
    dev.mkNixago
      {
        data = { };
        output = ".sops.yaml";
        format = "yaml";
        packages = with pkgs; [
          age
          sops
          ssh-to-age
          ssh-to-pgp
        ];
      }
      {
        data =
          let
            bsc = {
              age = "age1h8jgr473q6vj9e8kannr0ljzreu7whc46qhjfpjxxkl4w38ny5esz6mk0v";
            };

            eustoma = {
              age = "age1lgy77wf7vxlvvv8lzsgmq6wgf43c4hl93ls2mw8pspmdcuzqvems7svu6t";
            };

            lilac = {
              age = "age1g5777szdqk5all8tq823v5gzzjag5j3xk92gy44e5rqz9ktya4hsnjr8m9";
            };
          in
          {
            creation_rules = [
              {
                path_regex = "^nix/soil/nixosSecrets/eustoma/secrets\.yaml$";
                key_groups = [ { age = [ eustoma.age ]; } ];
              }
              {
                path_regex = "^nix/soil/nixosSecrets/lilac/secrets\.yaml$";
                key_groups = [ { age = [ lilac.age ]; } ];
              }
              {
                path_regex = "^nix/soil/homeSecrets/bsc/secrets\.yaml$";
                key_groups = [ { age = [ bsc.age ]; } ];
              }
            ];
          };
      };

  treefmt = dev.mkNixago cfg.treefmt {
    data = {
      formatter = {
        nix = {
          command = "nixfmt";
          includes = [ "*.nix" ];
        };

        # shellcheck = {
        #   command = "shellcheck";
        #   includes = [ "*.sh" ];
        # };

        shfmt = {
          command = "shfmt";
          includes = [ "*.sh" ];
        };
      };
    };

    packages = with pkgs; [
      nixfmt-rfc-style
      shellcheck
      shfmt
    ];
  };
}
