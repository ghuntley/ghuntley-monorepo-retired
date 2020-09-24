# OpenLDAP by default uses a simple shalted SHA1-hash for passwords,
# which is less than ideal.
#
# It does however include a contrib module which adds support for the
# Argon2 password hashing scheme. This overrides then OpenLDAP build
# derivation to include this module.
{ pkgs, ... }:

pkgs.originals.openldap.overrideAttrs(old: {
  buildInputs = old.buildInputs ++ [ pkgs.libsodium ];

  postBuild = ''
    ${old.postBuild}
    make $makeFlags -C contrib/slapd-modules/passwd/argon2
  '';

  # This is required because the Makefile for this module hardcodes
  # /usr/bin/install, which is not a valid path - we want it to be
  # looked up from $PATH because it is included in stdenv.
  installFlags = old.installFlags ++ [ "INSTALL=install" ];

  postInstall = ''
    ${old.postInstall}
    make $installFlags install-lib -C contrib/slapd-modules/passwd/argon2
  '';

})
