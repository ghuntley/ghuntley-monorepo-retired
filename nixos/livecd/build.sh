export NIX_PATH="nixpkgs=${REPO_ROOT}/third_party/github.com/nixos/nixpkgs-channels"

#nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=default.nix
nix-build '<nixpkgs/nixos>' -A system.build.qcow -I nixos-config=default.nix
