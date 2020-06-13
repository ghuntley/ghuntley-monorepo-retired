let
  # Pinned nixpkgs for vscode
  nixpkgsVscode = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs";
    rev = "f75d62941d8ad616bfc334ae19d71a0a6677dd19";
    ref = "master";
  };
  pkgsVscode = import nixpkgsVscode {};

  # Pinned for ghc and tools (unstable has cabal 3.2 which does not work with cabal-helper and hie)
  nixpkgs = builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs-channels";
    rev = "6d68b920eb2df73d68a9355f9d572dbf97add5f5";
    ref = "nixos-20.03";
  };
  pkgs = import nixpkgs {};

  # Haskell IDE engine (with ghc pinned to nixos-20.03 so it has new glibc)
  allHiesSrc = builtins.fetchGit {
    url = "https://github.com/atopuzov/all-hies/";
    rev = "f8249043a53f0f8e5f5415c0b47a28e00fcf5558";
    ref = "nixos-20.03";
  };
  allHies = import allHiesSrc { pkgs = pkgs; };

  hie = allHies.selection { selector = p: { inherit (p) ghc865; }; };

  compiler = pkgs.haskell.packages.ghc865;

  stack = pkgs.stdenv.mkDerivation {
    name = "stack-system-ghc";
    builder = pkgs.writeScript "stack" ''
      source $stdenv/setup
      mkdir -p $out/bin
      makeWrapper ${compiler.stack}/bin/stack \
        $out/bin/stack \
        --add-flags "--system-ghc --no-nix --no-docker"
    '';
    buildInputs = [ pkgs.makeWrapper ];
  };

  vscode = pkgsVscode.vscode-utils.vscodeEnv {
    user-data-dir = "./.vscode-globalUserData";
    mutableExtensionsFile = ./mutable-extensions.nix;
    nixExtensions = [
      { name = "vscode-hie-server"; publisher = "alanz"; version = "0.0.40"; sha256 = "1cmlgidjma41s5zq5161gcxxmk5lfzcm8dvznls04y5l7q9b0gca"; }
      { name = "language-haskell"; publisher = "justusadam"; version = "3.2.0"; sha256 = "190h1hky2yy5n00ncqf15mmaizgpm3w9pzvasmi2gangpg4qb6y5"; }
    ];
    settings = {};
    keybindings = {};
  };

  project = compiler.callPackage ./demo.nix {};
in

project.env.overrideAttrs (shellEnv: {
  buildInputs = shellEnv.buildInputs ++ [
    pkgs.which # needed for --pure
    compiler.cabal-install
    pkgs.cabal2nix
    hie
    vscode
  ];

  shellHook = ''
    echo "Ready!"
    # code --folder-uri $PWD
  '';
})
