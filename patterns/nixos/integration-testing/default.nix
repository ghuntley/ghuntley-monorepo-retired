{ pkgs ? import <nixpkgs> {} }: {
  test = pkgs.nixosTest ./test.nix;
}
