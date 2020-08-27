# Introducing the nixstore

## 📖 Overview

Most of the popular operating systems handle packages well, until, they don’t.
As long as you are moving in a straight line, alone, you’ll be fine. Things
change, when you introduce other people in the walk. For the whole cast to move
in unison, everyone must be strictly connected to one another. If a member
decides to break off, and walk on his own, the entire cast becomes crippled.
However, if that member clones himself so that the departing copy becomes
independent, the original walking cast becomes undisturbed.

Let’s take the case of a distribution aimed as a multi-user production
development environment. When you install Firefox version 100, the main binary
goes to either `/usr/bin/firefox` or `/usr/local/bin/firefox`. All the users
then, in this system, will be able to access the application from that path;
John, Mary, and Peter are happy. 

However, when John upgrades it to version 200, the same application that is
being used by Mary and Peter get upgraded, too! That’s not a good thing if they
prefer the old version that works with them! Nixpkgs allows you have multiple
versions of a software, without collisions from the other versions. John, Mary,
and Peter can all have their versions of Firefox without conflicting with the
other versions.

How does Nix do it? It does it by naming components by the computed checksum of
their inputs, and by not using a common global location.

Each user has their own versions of `~/.nix-profile`, and all of the contents of
those directories do not contain regular files. Instead, they are all symbolic
files to the actual files located in `/nix/store/`. 

This directory is where programs and their dependencies are actually installed.
The only way to write to that directory is through the Nix-specific programs.
There is no way to modify the contents of that directory through normal means.
So, when regular user john installs Vim 8, the program becomes installed as
something like `/nix/store/w4cr4j13lqzry2b8830819vdz3sdypfa-vim-8.0.0329`. The
characters before the package name is the checksum of all the inputs to build
the package.

The file `/home/john/.nix-profile/bin/vim` then points to a symlink to a file in
`/nix/store/` that will lead to the actual Vim binary in
`/nix/store/w4cr4j13lqzry2b8830819vdz3sdypfa-vim-8.0.0329/bin/vim`.

## 🎯 Activities

Let's start by checking that what we just said is true:

```bash
$ which bash
/run/current-system/sw/bin/bash
```

Yes, our `bash` is not where we'd expect it to be in the LFS. But is this bash
executable even a real file?

```bash
$ ls -l `which bash`
lrwxrwxrwx 1 root root 77 Jan  1  1970 /run/current-system/sw/bin/bash -> /nix/store/93h01q6yg13xdrabvqbddzbk11w6a928-bash-interactive-4.4-p23/bin/bash
```

No, it's not! It's a symbolic link to a path in the Nix store.

Note that the symbolic link is in `/run/current-system/sw/bin/`, which means
that our current user is running the same bash as the underlying NixOS. But
imagine that, for whatever reason, the user needs to run a different version of
Bash.

```bash
$ nix-env -iA nixos.bash
installing 'bash-4.4-p23'
these paths will be fetched (0.51 MiB download, 2.47 MiB unpacked):
  /nix/store/6bqvbzjkcp9695dq0dpl5y43nvy37pq1-bash-4.4-p23-info
  /nix/store/r7fng3kk3vlpdlh2idnrbn37vh4imlj2-bash-4.4-p23-man
  /nix/store/rjxwxwv1fpn9wa2x5ssk5phzwlcv4mna-bash-4.4-p23-doc
  /nix/store/zf5lbh336mnzf1nlswdn11g4n2m8zh3g-bash-4.4-p23-dev
copying path '/nix/store/rjxwxwv1fpn9wa2x5ssk5phzwlcv4mna-bash-4.4-p23-doc' from 'http://dymaxion.local'...
copying path '/nix/store/6bqvbzjkcp9695dq0dpl5y43nvy37pq1-bash-4.4-p23-info' from 'http://dymaxion.local'...
copying path '/nix/store/zf5lbh336mnzf1nlswdn11g4n2m8zh3g-bash-4.4-p23-dev' from 'http://dymaxion.local'...
copying path '/nix/store/r7fng3kk3vlpdlh2idnrbn37vh4imlj2-bash-4.4-p23-man' from 'http://dymaxion.local'...
building '/nix/store/9s8w4mp9x76wbkpbafgww6p3755rppb3-user-environment.drv'...
warning: skipping dangling symlink '/nix/store/fah9bdy13ph1h4fmp49cy0jngy1a7q6q-user-environment/include/pypy2.7'
created 3517 symlinks in user environment
```

Let's see which `bash` we're running now:

```bash
$ which bash
/home/kandinski/.nix-profile/bin/bash
```

We're now running our own instance of it. Let's see which file it links to:

```bash
$ ls -l `which bash`
lrwxrwxrwx 1 root root 65 Jan  1  1970 /home/kandinski/.nix-profile/bin/bash -> /nix/store/xfghy8ixrhz3kyy6p724iv3cxji088dx-bash-4.4-p23/bin/bash
```

Notice that the hash part of the store path has changed, while the version
number hasn't. This is because a package's store path is named after the hash of
**all its inputs**. The version number hasn't changed, which makes us think that
it's not the source code to bash that has changed, but maybe its runtime
dependencies, or its build dependencies such as the compiler.

That's right: other, lesser package managers reuse the same package name for
binaries that aren't identical. Under Nix, different files will have different
names.

There is a tradeoff though. Check out this example of the amount of cruft that
can accumulate in the /nix/store over time:

```bash
$ fd "bash-4.4-p23$" /nix/store
/nix/store/7bkv33x88dc64yhjvfvjvh0fqyhlliwj-bash-4.4-p23
/nix/store/six2s0711cs2878yhv7l5lgwzba2xqs9-bash-4.4-p23
/nix/store/czx8vkrb9jdgjyz8qfksh10vrnqa723l-bash-4.4-p23
/nix/store/0ds7prxc5w2wa9hx9r9vmxz6dp3lpsj3-bash-4.4-p23
/nix/store/2ch3pwfv40ls6qhdmzwsz9wky9pq6cyl-bash-4.4-p23
/nix/store/79b6s2wql94lwk8dyib2jc4fq8ixl13f-bash-4.4-p23
/nix/store/a9i0a06gcs8w9fj9nghsl0b6vvqpzpi4-bash-4.4-p23
/nix/store/b9p787yqaqi313l9rr0491igjwyzqfmw-bash-4.4-p23
/nix/store/vqq275dnd5lbjfdxffa8864w4mxrinx2-bash-4.4-p23
/nix/store/x0mlaj4z4ciycaycfwc36l1932mwywfj-bash-4.4-p23
/nix/store/cinw572b38aln37glr0zb8lxwrgaffl4-bash-4.4-p23
/nix/store/xfghy8ixrhz3kyy6p724iv3cxji088dx-bash-4.4-p23
/nix/store/53wi068kjrqfr2j0hzcxhbw2xaa990jr-bash-4.4-p23
```

13 different binary versions of the same version of the same shell!

Or the output of `ncdu` on `/nix/store`:

```bash
ncdu 1.14 ~ Use the arrow keys to navigate, press ? for help
--- /nix/store -----------------------------------------------------------------
    1.4 GiB [##########] /q4q4smqwvpla27gbvjzjy5mlymm0q0c3-ghc-8.2.2-binary
    1.4 GiB [######### ] /r0r77in17z6xynbspcpqlmxplvwbz5pc-ghc-8.6.5
    1.4 GiB [######### ] /44zyylbbhabj2d62f7lbb6wmjlllcv10-ghc-8.6.5
    1.4 GiB [######### ] /zs0j8a1rzj1s9n129j05mhc8ds32q9xn-ghc-8.6.5
    1.4 GiB [######### ] /g92mcyzzyi305cpl620xxanax5k3fii4-ghc-8.6.4
    1.4 GiB [######### ] /skl173whx3jg5da5qhs5bpj00ff1gvkj-ghc-8.6.5
    1.3 GiB [######### ] /gvshp9yvc6gql09r3cyryj2zgsnfk6br-ghc-8.6.4
    1.3 GiB [######### ] /s6jzapyn3wm4c38pb2c2sb8k6v3g83n7-ghc-8.6.4
    1.3 GiB [######### ] /8vq01xmxlp9wxzilkw85rb621ag7nwmd-ghc-8.6.4
    1.1 GiB [#######   ] /6hpn8gzkwwvwzlyb6rnwlhlqacl9zbzi-ghc-8.2.2
    1.1 GiB [#######   ] /zncdaxamqaf5l9pz1aw23a7wmcj93crx-ghc-8.2.2
    1.1 GiB [#######   ] /w8ckird699qc09np3bc3h47r5g86ab8s-ghc-8.2.2
    1.1 GiB [#######   ] /627cyfzpvmd8n4nqgksxb0mvh1wbz7s1-tesseract-3.05.00
    1.1 GiB [#######   ] /vicinv4i2nvdk6cxpy8h7wym8g3dybsp-all
    1.0 GiB [######    ] /w2nj8hqqg8wyk4ci7yri8sgrn1akimwf-ghc-8.0.2
    1.0 GiB [######    ] /nv6ym6wiv4ssqy81v6kjc30yrb53b5zv-wine-wow-4.9-staging
    1.0 GiB [######    ] /l2v99sydpf23pn0gyrdr82hx...g3w-wine-wow-4.12.1-staging
  739.0 MiB [#####     ] /fs525bmwbzjpnmnmi2lscfan...i2g-google-fonts-2018-07-13
  673.7 MiB [####      ] /3bfgswkdz559y7n1mxpnbrpf...cabal-at-2019-07-29T000000Z
  639.4 MiB [####      ] /fvkaqawargzj3m9s0rjnx7s6...yis-google-fonts-2017-06-28
  591.2 MiB [####      ] /k19zskg6lrpqwc80dinri01x613iangi-hackage-exprs-source
 Total disk usage: 101.1 GiB  Apparent size: 100.3 GiB  Items: 3125663
```

Your results may vary. In fact, they will! Try it yourselves.

If you don't have `ncdu`, maybe you can install it. It's a good tool that makes
sense to have available to your system, so maybe edit your
`/etc/nixox/configuration.nix`, add it to your list of packages in 
`environment.systemPackages` and then run `# nixos-rebuild switch`.

But some of these paths may not be referenced by any package or profile. We can
clean them with `nix-collect-garbage`:

```bash
$ nix-collect-garbage
finding garbage collector roots...
<...lots more...>
deleting '/nix/store/j17wcw4qbbrlqli8pnsrpzfzkb6vmyyj-bash44-004.drv'
deleting '/nix/store/7d2jgsd6bcxfha1g9kqv1wqw6w7ncl5j-login.defs'
deleting '/nix/store/p0fk3vrjgjazlrlm9vbkv4hkvhx6ikca-eog-3.26.2.drv'
deleting '/nix/store/8p0ll2b8m78j37x8m78aa43z592m5gdl-libpeas-1.22.0.drv'
deleting '/nix/store/trash'
deleting unused links...
note: currently hard linking saves -0.00 MiB
9611 store paths deleted, 50551.28 MiB freed
```

This is good... but not enough. The reason is that some of these paths are still accessible from previous generations of either the system environment or my own user environment. Check this out:

```bash
$ nix-env --list-generations
 135   2019-07-30 13:55:33   
 136   2019-08-02 09:38:21   
 137   2019-08-02 14:41:48   
 138   2019-08-02 14:59:43   
 139   2019-08-02 15:02:17   
 140   2019-08-02 15:02:24   
 141   2019-08-02 15:06:51   
 142   2019-08-02 17:39:29   
 143   2019-08-11 13:30:24   
 144   2019-08-18 22:20:37   
 145   2019-08-19 20:47:54   
 146   2019-08-19 20:51:15   
 147   2019-08-19 20:51:41   
 148   2019-08-19 20:57:32   
 149   2019-08-19 21:04:37   
 150   2019-08-21 21:49:06   
 151   2019-08-21 22:00:11   
 152   2019-08-27 01:36:45   (current)
```

In the past month (roughly since I started working on this tutorial) I've
updated my user environment almost 20 times. I could roll back these generations
if I wanted to run the software that was up-to-date then, instead of the
software that's up-to-date now. I bet there are some versions referenced there
that I no longer want or need. 

I can delete these generations of my profile, and then garbage-collect all of
`/nix/store` paths that are no longer accessible:

```bash
$ nix-collect-garbage --delete-old
removing old generations of profile /nix/var/nix/profiles/per-user/kandinski/profile
removing old generations of profile /nix/var/nix/profiles/per-user/kandinski/channels
finding garbage collector roots...
deleting garbage...
<...lots of paths here...>
deleting '/nix/store/trash'
deleting unused links...
note: currently hard linking saves -0.00 MiB
0 store paths deleted, 53552.17 MiB freed
```

And now I can't rollback my user environment, because I only have one.

```bash
$ nix-env --list-generations
 152   2019-08-27 01:36:45   (current)
```

And now my nix-store has lost 60Gb:

```bash
$ du -sh /nix/store
41G	/nix/store
```

I definitely don't want to do this by hand. Let's automate it by editing
`/etc/nixos/configuration.nix`:

```nix
# Enable automatic garbage collection
nix.gc = {
  automatic = true;
  dates = "daily";
  options = "--delete-older-than 30d";
};
```

Remember to run `nixos-rebuild switch` after you change your configuration.

## 📚 Additional reading material

- [A Gentle Introduction to the Nix Family](https://ebzzry.io/en/nix)

## ⏭️ What's next
