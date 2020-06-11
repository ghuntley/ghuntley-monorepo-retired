{ pkgs ? (import <nixpkgs> {})}:

let
  toolchain = with pkgs.rustChannels.stable;
    (rust.override {
     extensions = [ "rust-src" ];
  });
in pkgs.mkShell {
  buildInputs = with pkgs; [
    pkgs.jetbrains.idea-community
    toolchain
    pkg-config
    openssl
  ];
  shellHook = ''
    ln -sf ${toolchain} .toolchain
  '';
 }