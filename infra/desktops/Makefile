rebuild:
NIX_PATH="home-manager/nixos=${REPO_ROOT}/third_party/home-manager/nixos:nixpkgs=${REPO_ROOT}/code/third_party/nixpkgs:nixos-config=/etc/nixos/configuration.nix:/nix/var/nix/profiles/per-user/root/channels" nixos-rebuild switch

fmt:
find modules/ -name "*.nix" -exec nixfmt {} \;
nixfmt *.nix
