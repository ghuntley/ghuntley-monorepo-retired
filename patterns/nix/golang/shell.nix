{ pkgs ? import ./.nix/nixpkgs.nix { } }:

let
  drv = pkgs.callPackage ./default.nix {};
in
  drv.overrideAttrs (attrs: {
    nativeBuildInputs = attrs.nativeBuildInputs ++ [ 
      # Go tools
      pkgs.goimports
      pkgs.gotools
      pkgs.vgo2nix
    ];

    shellHook = ''
      set -euo pipefail

      export GOPATH="$(pwd)/.go"
      export GOCACHE=""
      export GO111MODULE='on'
      [ -f go.mod ] || go mod init ${attrs.goPackagePath}

      set +euo pipefail
    '';
  })
