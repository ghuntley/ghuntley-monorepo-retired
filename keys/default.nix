# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{
  hosts = {
    metabox = (builtins.readFile ./hosts/metabox.pub);
  };
  users = {
    ghuntley =
      {
        pgp = (builtins.readFile ./users/ghuntley.asc);
        ssh = (builtins.readFile ./users/ghuntley.pub);
      };
    mgmt =
      {
        pgp = (builtins.readFile ./users/ghuntley.asc);
        ssh = (builtins.readFile ./users/ghuntley.pub);
      };
  };
}
