let
  nixpkgs = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs";
    rev = "f75d62941d8ad616bfc334ae19d71a0a6677dd19";
    ref = "master";
  };

  # mozSrcMaster = builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz;
  mozSrc =  builtins.fetchGit {
    url = "https://github.com/mozilla/nixpkgs-mozilla";
    rev = "e912ed483e980dfb4666ae0ed17845c4220e5e7c";
    ref = "master";
  };
  moz = import mozSrc;
  pkgs = import nixpkgs { overlays = [ moz ]; };

  rust = pkgs.latest.rustChannels.stable.rust.override {
    extensions = [
      "rust-src"
      "rust-std"
      "rls-preview"
      "rust-analysis"
      "rustfmt-preview"
      "clippy-preview"
    ];};

  vscode = pkgs.vscode-utils.vscodeEnv {
    user-data-dir = "./.vscode-globalUserData";
    mutableExtensionsFile = ./mutable-extensions.nix;
    nixExtensions = [
      { name = "rust"; publisher = "rust-lang"; version = "0.7.8"; sha256 = "039ns854v1k4jb9xqknrjkj8lf62nfcpfn0716ancmjc4f0xlzb3";  }
      # { name = "cpptools"; publisher = "ms-vscode"; version = "0.28.2"; sha256 = "1cqqxxr7c7qs91b58ax4dyxd9qw0vskdqan39d84288wlwkgxwmf";  }
      # { name = "debug"; publisher = "webfreak"; version = "0.25.0"; sha256 = "0qm2jgkj17a0ca5z21xbqzfjpi0hzxw4h8y2hm8c4kk2bnw02sh1";  }
      { name = "better-toml"; publisher = "bungcip"; version = "0.3.2"; sha256 = "08lhzhrn6p0xwi0hcyp6lj9bvpfj87vr99klzsiy8ji7621dzql3";  }
    ];
    settings = {
      "rust-client.disableRustup"= true;
    };
    keybindings = {};
  };
in pkgs.mkShell {
  buildInputs = with pkgs; [
    vscode
    rust
    openssl pkgconfig nasm cmake zlib
  ];

  RUST_BACKTRACE="1";

  shellHook = ''
    export PATH="target/debug/:$PATH"
    echo "Ready!"
  '';
}
