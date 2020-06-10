# This file controls the import of external dependencies (i.e.
# third-party code) into my package tree.
#
# This includes *all packages needed from nixpkgs*.
{ ... }:

let
  # Tracking nixos-unstable as of 2020-05-21.
  nixpkgsCommit = "0f5ce2fac0c726036ca69a5524c59a49e2973dd4";
  nixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/${nixpkgsCommit}.tar.gz";
    sha256 = "0nkk492aa7pr0d30vv1aw192wc16wpa1j02925pldc09s9m9i0r3";
  };
  nixpkgs = import nixpkgsSrc {
    config.allowUnfree = true;
    config.allowBroken = true;
  };

  # Tracking nixos-20.03 as of 2020-05-22
  stableCommit = "48723f48ab92381f0afd50143f38e45cf3080405";
  stableNixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/${stableCommit}.tar.gz";
    sha256 = "0h3b3l867j3ybdgimfn76lw7w6yjhszd5x02pq5827l659ihcf53";
  };
  stableNixpkgs = import stableNixpkgsSrc {};

  exposed = {
    # Inherit the packages from nixos-unstable that should be available inside
    # of the repo. They become available under `pkgs.third_party.<name>`
    inherit (nixpkgs)
      age
      autoconf
      autoreconfHook
      bashInteractive
      bat
      buildGoModule
      buildGoPackage
      buildPackages
      bzip2
      c-ares
      cacert
      cachix
      cairo
      cargo
      cgit
      clang-tools
      clang_10
      cmake
      coreutils
      cudatoolkit
      darwin
      dockerTools
      fetchFromGitHub
      fetchurl
      fetchzip
      fira
      fira-code
      fira-mono
      fontconfig
      freetype
      gettext
      glibc
      gmock
      gnutar
      go
      google-cloud-sdk
      graphviz
      gzip
      haskell
      iana-etc
      imagemagickBig
      jetbrains-mono
      jq
      kontemplate
      lib
      libredirect
      luajit
      luatex
      makeFontsConf
      makeWrapper
      mdbook
      meson
      mime-types
      moreutils
      nano
      nginx
      ninja
      nix
      openssh
      openssl
      overrideCC
      pandoc
      parallel
      pkgconfig
      pounce
      python3
      python3Packages
      remarshal
      rink
      ripgrep
      rsync
      runCommand
      runCommandNoCC
      rustPlatform
      rustc
      sbcl
      sqlite
      stern
      symlinkJoin
      systemd
      tdlib
      terraform_0_12
      texlive
      thttpd
      tree
      which
      writeShellScript
      writeShellScriptBin
      writeText
      writeTextFile
      xorg
      xz
      zlib
      zstd;

    # Inherit packages that should come from a stable channel
    inherit (stableNixpkgs)
      emacs26
      emacs26-nox
      emacsPackages
      emacsPackagesGen;

    # Required by //third_party/nix
    inherit (nixpkgs)
      aws-sdk-cpp
      bison
      boehmgc
      boost # urgh
      brotli
      busybox-sandbox-shell
      curl
      docbook5
      docbook_xsl_ns
      editline
      flex
      libseccomp
      libsodium
      libxml2
      libxslt
      mercurial
      perl
      perlPackages
      utillinuxMinimal;
  };

in exposed.lib.fix(self: exposed // {
  callPackage = nixpkgs.lib.callPackageWith self;

  # Provide the source code of nixpkgs, but do not provide an imported
  # version of it.
  inherit nixpkgsCommit nixpkgsSrc stableNixpkgsSrc;

  # Packages to be overridden
  originals = {
    inherit (nixpkgs) grpc notmuch;
    inherit (stableNixpkgs) git;
    ffmpeg = nixpkgs.ffmpeg-full;
  };

  # Use LLVM 10
  llvmPackages = nixpkgs.llvmPackages_10;
  clangStdenv = nixpkgs.llvmPackages_10.stdenv;
  stdenv = nixpkgs.llvmPackages_10.stdenv;

  # Make NixOS available
  nixos = import "${stableNixpkgsSrc}/nixos";
})
