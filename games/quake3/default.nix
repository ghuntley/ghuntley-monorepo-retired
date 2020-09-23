# Copyright (c) 2020 Geoffrey Huntley <ghuntley@ghuntley.com>. All rights reserved.
# SPDX-License-Identifier: Proprietary

{ depot, pkgs, lib, ... }:

with pkgs;
let paks = stdenv.mkDerivation rec {
  pname = "quake3-paks";
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "rvolosatovs";
    repo = "ioquake3-mac-install";
    rev = "91e37f075ebf65510e130981bdfdbfdc47265938";
    sha256 = "1rdjfcqp4df1cazgbkv6bcj5ddfg8ggg96kjickynnxw7xjxjanf";
  };

  # buildInputs = [ ioquake3 quake3e quake3pointrelease ];

  buildCommand = ''
    cat $src/dependencies/baseq3/pak0/pak0.z01 \
        $src/dependencies/baseq3/pak0/pak0.z02 \
        $src/dependencies/baseq3/pak0/pak0.z03 \
        $src/dependencies/baseq3/pak0/pak0.z04 \
        $src/dependencies/baseq3/pak0/pak0.zip > pak0-master.zip
    ${unzip}/bin/unzip -a pak0-master.zip || true
    install -Dm444 pak0.pk3 $out/baseq3/pak0.pk3
    install -Dm444 $src/dependencies/baseq3/pak1.pk3 $out/baseq3/pak1.pk3
    install -Dm444 $src/dependencies/baseq3/pak2.pk3 $out/baseq3/pak2.pk3
    install -Dm444 $src/dependencies/baseq3/pak3.pk3 $out/baseq3/pak3.pk3
    install -Dm444 $src/dependencies/baseq3/pak4.pk3 $out/baseq3/pak4.pk3
    install -Dm444 $src/dependencies/baseq3/pak5.pk3 $out/baseq3/pak5.pk3
    install -Dm444 $src/dependencies/baseq3/pak6.pk3 $out/baseq3/pak6.pk3
    install -Dm444 $src/dependencies/baseq3/pak7.pk3 $out/baseq3/pak7.pk3
    install -Dm444 $src/dependencies/baseq3/pak8.pk3 $out/baseq3/pak8.pk3
    install -Dm444 $src/dependencies/baseq3/q3key $out/baseq3/q3key
    install -Dm444 $src/extras/extra-pack-resolution.pk3 $out/baseq3/pak9hqq37test20181106.pk3
    install -Dm444 $src/extras/quake3-live-sounds.pk3 $out/baseq3/quake3-live-soundpack.pk3
    install -Dm444 $src/extras/hd-weapons.pk3 $out/baseq3/pakxy01Tv5.pk3
  '';
};
in
quake3wrapper {
  paks = [ paks ioquake3 quake3pointrelease ];
}
