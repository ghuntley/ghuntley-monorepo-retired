---
layout: notes
title: nixos
---
Only 1 hoogle?! That’s sad.

nix-shell -p "haskellPackages.ghcWithHoogle (pkgs: with pkgs; [ lens ])" --run "hoogle server --local --port 8080"

and you’re up and running! With incremental search and everything local. https://t.co/LqduSBEHFH
# running unpatched binaries
- https://brianmckenna.org/blog/running_binaries_on_nixos


# docker
https://github.com/puffnfresh/nix-files/blob/master/nixos-docker/default.nix

# stuff

https://github.com/shajra/example-nix/blob/master/README.md

# private cache
* http://softwaresimply.blogspot.com/2018/07/setting-up-private-nix-cache.html
* https://nixos.wiki/wiki/FAQ/Private_Cache_Proxy

nixos in production at http://www.haskellforall.com/2018/08/nixos-in-production.html

Great introduction over at https://ebzzry.io/en/nix/

{{< tweet 978211520189554688 >}}

# Reproducable Builds
* https://github.com/basvandijk/nixtodo/blob/master/nixpkgs.nix

# how are channels built?

https://howoldis.herokuapp.com


# Terminology

# determine sha of fetch

```
$ nix-prefetch-url --unpack https://github.com/dotnet/coreclr/archive/v2.1.1.tar.gz
unpacking...

warning: dumping very large path (> 256 MiB); this may run out of memory
path is '/nix/store/a6xaij9sld927ilfdy5mvf8zjw91jzp4-v2.1.1.tar.gz'
1qp1kp04xdxwhcj66i0jz94cx9bp9gqvq8mlpn84p6yzskkryiln
```

# nixos-build

```
$ cat default.nix
with import <nixpkgs> {};
callPackage ./coreclr.nix {}
```

# nixos-rebuild

## Disabling Modules

Use `disabledModules` to prevent nix from importing an module imported from nixpkgs

```nix
{ config, lib, pkgs, ... }:
with lib;
{
  disabledModules = [ "security/duosec.nix" ];
  imports = [ ./duosec-patched-module.nix ];
}
```

# nix-shell

## Installing project dependancies

When `nix-shell` is invoked it looks for `default.nix` and `shell.nix` and evaluates the contents. This is useful for locally installing project dependancies.

```nix
with (import <nixpkgs> {});
stdenv.mkDerivation {
  name = "slides";
  buildInputs = [ tectonic ];
}
```

# Converting string to bool

depressing amount of `${if x then "true" else "false"}` in nixpkgs :(

```
nix-repl> builtins.toString true -> "1"
nix-repl> builtins.toString false -> ""
```

# Internals

`nixos\modules\installer\tools\` is where `nixnos-rebuild.sh` is located. 

Concepts:
* `nix-build` compiles the operating system into a closure
* `nix-copy-closure`copies closures.
* `nixos-generate-config.pl` generates the initial `configuration.nix and the `hardware-configuration.nix` by probing the system for hardware.


```
Useful flags for nixos-install:

--no-root-passwd)
    noRootPasswd=1
    ;;
--no-bootloader)
    noBootLoader=1
    ;;
```

```
Contents of the installation CD

Mode                LastWriteTime         Length Name
----                -------------         ------ ----
d-----        1/01/1970  11:00 AM                BOOT
d-----        1/01/1970  11:00 AM                EFI
d-----        1/01/1970  11:00 AM                ISOLINUX
d-----        1/01/1970  11:00 AM                LOADER
--r---        1/01/1970  11:00 AM     1024208896 NIX_STORE.SQUASHFS
--r---        1/01/1970  11:00 AM             24 VERSION.TXT
--r---        1/01/1970  11:00 AM           2048 _BOOT.CAT
```

```
Contents of the NIX_STORE.SQUASHFS:

/[the-nix-closures]/**
15kgcm8hnd99p7plqzx7p4lcr2jni4df-set-source-date-epoch-to-latest.sh
17x7j4dkm2r73i34x8fk2a44w6n83694-fc-00-nixos-cache.conf
28h69kg0zbzv410m759qmnqdwxyiagwf-client.conf
4rd3hzbjjiwpm91a87qb4qk2ppqsgnj9-relaxedsandbox.nix
55277z2q1441i4719n0c25sf6aba0v6w-etc-nsswitch.conf
786y0ys9am0fhl9r4dfxdfj7skcz3x0n-fc-00-nixos-cache.conf
81ikflgpwzgjk8b5vmvg9gaw9mbkc86k-compress-man-pages.sh
90x191ry47bwa2ynjrgz547d5ai27hq3-nix.conf
9bzcmp322gyjli8lyr4ah073dy0v4ly6-etc-resolvconf.conf
9ny6szla9dg61jv8q22qbnqsz37465n0-multiple-outputs.sh
9xjpy7da9jkxrzw4ikc8vjkd012a6qyl-configuration.nix
a92kz10cwkpa91k5239inl3fd61zp5dh-move-lib64.sh
amiqj6qasqrmp633zxqxsy7gvs93vjz8-etc-nscd.conf
dg6pg2icy36p6ci16zm76gglff8zlkwv-kmod-debian-aliases-22-1.1.conf
fxjxdnrbyq8l8r7sb0lm8y3875420vsr-etc-logind.conf
g5640z1ymqlxgmllpbc3ypfkc5dqpvgc-etc-nixos.conf
gpy3r9ss5ngfkib8ylx7jzgahq7m0x5b-patch-shebangs.sh
grcygmlkv3nvia6n03kx9ac4v4ib1682-mounts.sh
h0kvjl72iba3z38g3qa9r5lf3xaqgj5d-etc-journald.conf
h3rrrpg12l7lddb7958546y4679xdnsr-etc-system.conf
hjbv2s4jcxpvq07lxl4dna860rsdd3qf-multiuser.nix
hwq767zz2ppld30bqi4aqzpkqaqni9pc-smb-dummy.conf
j1q2bm9qjrhr6q2z819dgzi7bmy3hhjq-audit-tmpdir.sh
j8qx7gyxdxal025fac3xh2v4w99b0jwk-etc-nixos.conf
jw961avfhaq38h828wnawqsqniasqfwz-strip.sh
k2a5d25y3j6jlracj1869725n48mgkwi-fc-10-antialias.conf
k9m5dhhz40rd6d5y1lil0q7dx74i4ki9-locale.conf
kjr2p615jblvxmvglh3844hcji3905b5-vconsole.conf
l0hbkwps0bkvism0skkpiz4aj90x91lg-etc-host.conf
mjjy30kxz775bhhi6j9phw81qh6dsbrf-move-docs.sh
n66c9z2zcd0qga22lf2mzg6k5xwqy6p6-etc-sleep.conf
nhwc8sizhpxcg8qfjffdran77wf6fkp9-fc-10-hinting.conf
p9ib3gnygdh16gyjwmh58r4iss8ag88a-fc-10-subpixel.conf
ph49ghv2qah3bxvfzs52biyk2vy29577-fc-52-nixos-default-fonts.conf
pi49adk65j0g4f7zg5h0xgjchzpyvd09-etc-timesyncd.conf
q86mjh58xsk6mixfz4y1f1rihq6r5mjd-fonts.conf
rrjwl0j68vglsmvbcvasgx41swyafamb-etc-nixos.conf
xrhk3sx3dyhy9vq8pqh740nd0bhf0d1r-sandbox.nix
xyq5nm7ggqjd7c564ijiljxpzhqhv0w5-dhcpcd.conf
y5nphgqpv01kp2mx1pxks6wr0ygrpb5k-fc-53-no-type1.conf
z20kh7h5bq772dpdpmik2gvxns6zdik3-etc-user.conf
z82dl6ialp166drqihzkz67nkl6w3l16-move-sbin.sh
zzsw9j2h1icyx1817k8wmnv6ma5k2abc-nixos.conf
```

Default mounts:
```
specialMount "devtmpfs" "/dev" "nosuid,strictatime,mode=755,size=5%" "devtmpfs"
specialMount "devpts" "/dev/pts" "nosuid,noexec,mode=620,ptmxmode=0666,gid=3" "devpts"
specialMount "tmpfs" "/dev/shm" "nosuid,nodev,strictatime,mode=1777,size=50%" "tmpfs"
specialMount "proc" "/proc" "nosuid,noexec,nodev" "proc"
specialMount "tmpfs" "/run" "nosuid,nodev,strictatime,mode=755,size=25%" "tmpfs"
specialMount "ramfs" "/run/keys" "nosuid,nodev,mode=750,gid=96" "ramfs"
specialMount "tmpfs" "/run/wrappers" "nodev" "tmpfs"
specialMount "sysfs" "/sys" "nosuid,noexec,nodev" "sysfs"
```

# Working locally
```
$ mkdir -p ~/tmpdev && cd ~/tmpdev
$ git clone --depth=1 https://github.com/nixos/nixpkgs
$ export NIXPKGS=~/tmpdev/nixpkgs
$ ls $NIXPKGS

make some changes ...

example: list all available software from the local repository $NIXPKGS

$ nix-env -f $NIXPKGS -qaP '*'

example: install software from local repository

$ nix-env -f $NIXPKGS -i python-urlgrabber

example: update the system based on your local $NIXPKGS

$ nixos-rebuild -I nixpkgs=$NIXPKGS switch

example: build an expression and put the output in to `pwd`/results

$ nix-build $NIXPKGS -A irssi

example: get an environment which is used to build irssi (also see nix-shell)

$ nix-build $NIXPKGS --run-env -A irssi
```

{:footnotes}
