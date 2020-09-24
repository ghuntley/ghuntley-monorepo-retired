{ depot, pkgs, lib, ... }:

with pkgs;

stdenv.mkDerivation rec {
  version = "2.3.0";
  name = "licensed${version}";

  src = pkgs.fetchurl {
    url = "https://github.com/github/licensed/releases/download/${version}/licensed-${version}-linux-x64.tar.gz";
    sha256 = "sha256:1fhwv0dr5lip38hqvdc9qh28hsjhzb9df7przcg70g35a5qy33i3";
  };

  buildInputs = [ mktemp ];

  phases = [ "installPhase" ];

  installPhase = ''
    mkdir -p $out/bin
    tar -xvf ${src}
    cp licensed $out/bin/licensed
    chmod +x $out/bin/licensed
  '';

  meta = {
    homepage = https://github.com/github/licensed;
    description = "Licensed caches the licenses of dependencies and checks their status.";
    platforms = stdenv.lib.platforms.all;
    license = stdenv.lib.licenses.mit;
  };
}
