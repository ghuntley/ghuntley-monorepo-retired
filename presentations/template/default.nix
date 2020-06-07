# This derivation builds the LaTeX presentation.

{ pkgs, ... }:

with pkgs;

let tex = texlive.combine {
  inherit (texlive)
    beamer
    beamertheme-metropolis
    etoolbox
    euenc
    extsizes
    fontspec
    lualibs
    luaotfload
    luatex
    minted
    ms
    pgfopts
    scheme-basic
    translator;
};
in stdenv.mkDerivation {
  name = "nuug-bootstrapping-slides";
  src = ./.;

  FONTCONFIG_FILE = makeFontsConf {
    fontDirectories = [ fira fira-code fira-mono ];
  };

  buildInputs = [ tex fira fira-code fira-mono ];
  buildPhase = ''
    # LaTeX needs a cache folder in /home/ ...
    mkdir home
    export HOME=$PWD/home
    # ${tex}/bin/luaotfload-tool -ufv

    # As usual, TeX needs to be run twice ...
    function run() {
      ${tex}/bin/lualatex presentation.tex
    }
    run && run
  '';

  installPhase = ''
    mkdir -p $out
    cp presentation.pdf $out/
  '';
}
