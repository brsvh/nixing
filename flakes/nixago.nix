{ inputs
, lib
, ...
}:
{
  configurations = {
    default = {
      nixago = inputs.nixago;
    };
  };

  perSystem =
    { config
    , ...
    }:
    {
      configurations = {
        nixago = {
          configs =
            [
              {
                output = ".sops.yaml";
                format = "yaml";
                data =
                  let
                    bsc = {
                      age = "age1h8jgr473q6vj9e8kannr0ljzreu7whc46qhjfpjxxkl4w38ny5esz6mk0v";
                      pgp = "7B740DB9F2AC6D3B226BC53078D74502D92E0218";
                    };
                    eustoma = {
                      age = "age1lgy77wf7vxlvvv8lzsgmq6wgf43c4hl93ls2mw8pspmdcuzqvems7svu6t";
                    };
                  in
                  {
                    creation_rules =
                      [
                        {
                          path_regex = "^secrets/bsc\.yaml$";
                          key_groups = [
                            {
                              pgp = [ bsc.pgp ];
                              age = [ bsc.age ];
                            }
                          ];
                        }
                        {
                          path_regex = "^secrets/eustoma\.yaml$";
                          key_groups = [
                            {
                              age = [ eustoma.age ];
                            }
                          ];
                        }
                      ];
                  };
              }
            ];
        };
      };

      devshells = {
        default = {
          devshell = {
            startup = {
              nixago = {
                text = config.configurations.nixago.shellHook;
              };
            };
          };
        };
      };
    };
}
