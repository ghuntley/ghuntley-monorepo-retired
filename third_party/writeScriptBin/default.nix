{ depot, ... }:

# Like writeScript,
# but put the script into `$out/bin/${name}`.

name:
script:

depot.third_party.binify {
  exe = (depot.third_party.writeScript name script);
  inherit name;
}
