# Utility for invoking slappasswd with the correct options for
# creating an ARGON2 password hash.
{ pkgs, ... }:

pkgs.writeShellScriptBin "hash-password" ''
  ${pkgs.openldap}/bin/slappasswd -o module-load=pw-argon2 -h '{ARGON2}'
''
