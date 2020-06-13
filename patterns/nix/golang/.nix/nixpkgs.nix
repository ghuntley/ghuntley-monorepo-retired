import (
  builtins.fetchTarball {
    # NOTE: The 'release-20.03' commit below does not yet have a necessary fix
    # for the Golang compiler; instead, a recent 'nixpkgs-unstable' commit will
    # be used...
    #
    # https://github.com/NixOS/nixpkgs/issues/90136

    # # Latest commit on the 'release-20.03' branch as of 12 June 2020
    # url = "https://github.com/nixos/nixpkgs/archive/db31e48c5c8d99dcaf4e5883a96181f6ac4ad6f6.tar.gz";
    # sha256 = "1j5j7vbnq2i5zyl8498xrf490jca488iw6hylna3lfwji6rlcaqr";

    # Latest commit on the nixpkgs-unstable branch as of 12 June 2020
    url = "https://github.com/nixos/nixpkgs/archive/dcb64ea42e64aaecd8e6fef65cc86245c9666818.tar.gz";
    sha256 = "0i77sgs0gic6pwbkvk9lbpfshgizdrqyh18law2ji1409azc09w0";
  }
)
