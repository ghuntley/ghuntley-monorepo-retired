# Building Applications

## Overview

In a [previous section](../02-using-nix/), we got comfortable
with using Nix as a package manager. Now we’re going to get a feel for
what it’s like to build a package.

Most of this information is available in the [‘Writing Nix expressions’
chapter](https://nixos.org/nix/manual/#chap-writing-nix-expressions) of
the [Nix manual](https://nixos.org/nix/manual/), with more detail coming
from the [‘Standard Environment’
chapter](https://nixos.org/nixpkgs/manual/#chap-stdenv) of the [Nixpkgs
manual](https://nixos.org/nixpkgs/manual/). This is another of the Nix
manuals that you’ll get to know and love. In particular, the Nixpkgs
manual haschapters on specific languages and frameworks that can be
extremely helpful.

Just like last time, you should be able to play along with the examples
as you are reading this post.

An overview of building things with Nix
---------------------------------------

Let’s create a package for the GNU hello utility. We’re going to travel
quickly and gloss over some things but we’ll dig into the details
immediately afterwards. For now we’re going to set up an example package
and try to get a sense of what happens when Nix builds something
successfully.

To that end, we’ll write `hello.nix`:

    { stdenv, fetchurl }:

    stdenv.mkDerivation rec {
      name = "hello-2.10";

      src = fetchurl {
        url = "mirror://gnu/hello/${name}.tar.gz";
        sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
      };
    }

to package up the GNU hello utility.

This is a function that takes two arguments and produces a derivation -
and the derivation is what we are after.

There is a function in the Nix package set named `callPackage` that is
helpful here. The `callPackage` function will fill in missing arguments
to functions, provided that the argument names match the names of
packages from the Nix package set.

We can write `default.nix` to demonstrate this:

    let
      nixpkgs = import <nixpkgs> {};
    in
      nixpkgs.callPackage ./hello.nix {}

The missing arguments to `hello.nix` have been filled in by
`callPackage`, and so `default.nix` gives us a derivation.

The GNU hello utility is built with `autotools` - `configure`, `make`
and friends - and Nix will try to use those if you don’t give it any
other information about how to build a derivation.

This means we can build the derivation with:

    nix-build default.nix
    ...
    /nix/store/g0fw64zf7n0hr1dx7yl9n8qgqbhdngrm-hello-2.10

If no file is given to `nix-build` it will look for `default.nix` in the
current directory, so we can do:

    nix-build

and get the same result.

The output of the build is installed into the Nix store, but will also
appear in the symbolic link named `result`:

    > ls result
    bin 
    share

This symbolic link is set up as a garbage collection root, so your
package and its dependencies will stick around until you remove `result`
and a garbage collection occurs.

We could run the executable:

    > result/bin/hello
    Hello, world!

but we’d feel a bit bad for whoever had to muck about with autotools to
package something so simple, so maybe we shouldn’t.

We’re about to look at all of this in greater detail, but before we do
that we should clean up after ourselves:

    > rm result
    > nix-collect-garbage -d

What is going on under the hood
-------------------------------

### Getting hold of our sources

In order to make use of the tarball containing the GNU hello source
code, we need to know the hash of the sources.

Thankfully there are some helpful scripts for this, which we’ll install:

    nix-env -i nix-prefetch-scripts

We can use these scripts to download things, add them to the nix store,
and to print their hashes and other metadata that we might need.

Let’s grab a hold of those sources now:

    > nix-prefetch-url mirror://gnu/hello/hello-2.10.tar.gz
    downloading ‘http://ftpmirror.gnu.org/hello/hello-2.10.tar.gz’... [0/0 KiB, 0.0 KiB/s]
    path is ‘/nix/store/3x7dwzq014bblazs7kq20p9hyzz0qh8g-hello-2.10.tar.gz’
    0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i

We can see that there is a copy in the Nix store, and the script prints
out the hash of the tarball as well.

At the moment the prefetch scripts contain:

-   nix-prefetch-bzr
-   nix-prefetch-cvs
-   nix-prefetch-git
-   nix-prefetch-hg
-   nix-prefetch-svn
-   nix-prefetch-url

These have corresponding functions which we use inside our Nix packages:

    src = fetchurl {
      url = "mirror://gnu/hello/${name}.tar.gz";
      sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
    };

so that other people can use them without having to do any prefetching.

Once you know the hash of the sources, you are good to go. That is why
`default.nix` was able to download and build the sources at the start of
this post, before we did the prefetch.

There is one small optimization available for GitHub users which is
worth pointing out. Fetching from git will checkout the whole repository
in order to get hold of a specific revision:

    exitcode = pkgs.fetchgit {
      url = "https://github.com/qfpl/exitcode";
      rev = "e56313946fdfe77eed91c54d791b7be8aa73c495";
      sha256 = "0a2nnk9ifaln0v9dq0wgdazqxika0j32vv8p6s4qsrpfs2kxaqar";
    };

but f the project we are interested in is hosted on GitHub, there is a
function that can use the GitHub APIs to fetch the code for a specific
revision:

    exitcode = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "exitcode";
      rev = "e56313946fdfe77eed91c54d791b7be8aa73c495";
      sha256 = "0a2nnk9ifaln0v9dq0wgdazqxika0j32vv8p6s4qsrpfs2kxaqar";
    };

which can save us some time and data.

### Building the project from `nix-shell`

We have `nix-build` available to us for building Nix derivations.

While we’re working on writing our own derivations, we can use
`nix-shell`

    > nix-shell default.nix
    nix-shell >

If no file is given to `nix-shell` it will look for `shell.nix` in the
current directory, and then `default.nix`, so we can do:

    > nix-shell
    nix-shell >

By default we’ll have access to everything from our host environment
while we’re inside of the shell, so that we can edit files and so on. If
we want to make sure that we have specified all of the required
dependencies to build our package, we should use:

    > nix-shell --pure

so that the only things we have access to are the things mentioned in
our Nix files.

We would normally use separate `shell.nix` and `default.nix` files if we
wanted to tweak the packages and environment that is available while
we’re developing a package. This might include things like adding a
debugger or an editor as a dependency in `shell.nix`, or setting an
environment variable that leads to more verbose output as things build.

Let’s have a look at what we have at our disposal once we’re inside the
shell. Along with a few standard build tools, we have access to the
attributes from `hello.nix` as environment variables.

The `name` attribute is present:

    nix-shell> echo $name
    hello-2.10

The `src` attribute is present, and is a path to the sources in the Nix
store:

    nix-shell> echo $src
    /nix/store/3x7dwzq014bblazs7kq20p9hyzz0qh8g-hello-2.10.tar.gz
    nix-shell> ls $src
    /nix/store/3x7dwzq014bblazs7kq20p9hyzz0qh8g-hello-2.10.tar.gz

The hash of this derivation has already been calculated at this point,
so Nix has added an environment variable pointing to the directory where
we should put our results:

    nix-shell> echo $out
    /nix/store/g0fw64zf7n0hr1dx7yl9n8qgqbhdngrm-hello-2.10

but it hasn’t been created yet:

    nix-shell> ls $out
    ls: cannot access '/nix/store/g0fw64zf7n0hr1dx7yl9n8qgqbhdngrm-hello-2.10': No such file or directory

Give all of that information, we can use `autotools` to build the GNU
hello utility:

    nix-shell> tar zxf $src
    nix-shell> cd hello-*
    nix-shell> ./configure --prefix=$out
    nix-shell> make
    nix-shell> make install

and then run it directly from the Nix store:

    nix-shell> exit
    > /nix/store/g0fw64zf7n0hr1dx7yl9n8qgqbhdngrm-hello-2.10/bin/hello
    Hello, world!

Let us clean this up for our next little adventure:

    > nix-collect-garbage -d

### Building the project with `builder.sh`

We can collect these build steps into a bash script, which we’ll name
`builder.sh`:

    source $stdenv/setup

    tar zxvf $src
    cd hello-*
    ./configure --prefix=$out
    make
    make install

    echo "The script actually ran"

The `$stdenv/setup` step on the first line is setting up the Nix
environment for us. The echo on the last line is so that we can
distinguish this build from the automatic build steps that Nix did for
us during the overview.

If we reference this script from the `builder` attribute in our Nix
package:

    { stdenv, fetchurl }:

    stdenv.mkDerivation rec {
      name = "hello-2.10";
      
      builder = ./builder.sh;

      src = fetchurl {
        url = "mirror://gnu/hello/${name}.tar.gz";
        sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
      };
    }

then Nix will use it build the package:

    > nix-build
    ...
    The script actually ran
    /nix/store/p945zy2qhfbk6rb5gvyqclymyvwx5z7q-hello-2.10

This gives us a different hash to what we had before, which is expected
since the inputs have changed. If we changed the builder script:

    source $stdenv/setup

    echo "A different builder"

    tar zxvf $src
    cd hello-*
    ./configure --prefix=$out
    make
    make install

we can see that the builder script takes part in the hash computation as
well:

    nix-shell> echo $out
    /nix/store/7hd6g7hpdg21h5nrzijicim6lik9qsjr-hello-2.10

We can also inline the builder script by making use of some of the
utility functions that Nix provides along with the multi-line string
literals:

    { pkgs, stdenv, fetchurl }:

    stdenv.mkDerivation rec {
      name = "hello-2.10";
      
      builder = pkgs.writeText "builder.sh" ''
        source $stdenv/setup

        tar zxvf $src
        cd hello-*
        ./configure --prefix=$out
        make
        make install

        echo "The script actually ran"
      '';

      src = fetchurl {
        url = "mirror://gnu/hello/${name}.tar.gz";
        sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
      };
    }

### Debugging a nix build

We’re now going to sabotage our builder, to see what we can see what we
do to correct things that have gone awry.

We’ll deliberately change into the wrong directory:

    source $stdenv/setup

    tar zxvf $src
    cd hello
    ./configure --prefix=$out
    make
    make install

    echo "The script actually ran"

resulting in

    > nix-build
    ...
    /nix/store/s143n1fws6lb0ngnk6bm6ggrdxkxg8c8-builder.sh: line 4: cd: hello: No such file or directory
    builder for ‘/nix/store/p9lvjiik8pwv4rlpxsl29as151vrwp2z-hello-2.10.drv’ failed with exit code 1
    error: build of ‘/nix/store/p9lvjiik8pwv4rlpxsl29as151vrwp2z-hello-2.10.drv’ failed

What can we do?

We can indicate that we want to *keep* the temporary build directory in
the event of failures:

    > nix-build -K
    /nix/store/s143n1fws6lb0ngnk6bm6ggrdxkxg8c8-builder.sh: line 4: cd: hello: No such file or directory
    note: keeping build directory ‘/tmp/nix-build-hello-2.10.drv-0’
    builder for ‘/nix/store/p9lvjiik8pwv4rlpxsl29as151vrwp2z-hello-2.10.drv’ failed with exit code 1
    error: build of ‘/nix/store/p9lvjiik8pwv4rlpxsl29as151vrwp2z-hello-2.10.drv’ failed

We can have a look in this directory:

    > cd /tmp/nix-build-hello-2.10.drv-0
    > ls
    env-vars hello-2.10

and we’ll see that we have the unpacked sources for the GNU hello
utility, along with a file naamed `env-vars`.

This file contains the environment variables at the point of the
failure, so we can source that file:

    > source env-vars

and then try to carry out our build steps like we were in a `nix-shell`
to see what went wrong:

    > cd hello-2.10
    > configure --prefix=$out
    ....

### Building the project with the generic builder

Instead of specifying a build script, we can let Nix’s generic builder
do some of the work for us.

The generic builder proceeds through a number of phases, which we can
overload if we want:

    { stdenv, fetchurl }:

    stdenv.mkDerivation rec {
      name = "hello-2.10";

      src = fetchurl {
        url = "mirror://gnu/hello/${name}.tar.gz";
        sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
      };

      unpackPhase = ''
        tar zxvf $src
        cd hello-*
      '';

      configurePhase = ''
        ./configure --prefix=$out
      '';

      buildPhase = ''
        make
      '';

      installPhase = ''
        make install
      '';
    }

There is a default set of phases that get run, and there are default
activities that get run in each phase. We can edit the list of phases to
run, either to add new phases, to reorder them, or to skip some of them:

      phases = [installPhase];

Each of these phases has a default implementation that is usually pretty
sensible. We can see that by removing our `unpackPhase` and letting the
generic build functionality take over:

    { stdenv, fetchurl }:

    stdenv.mkDerivation rec {
      name = "hello-2.10";

      src = fetchurl {
        url = "mirror://gnu/hello/${name}.tar.gz";
        sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
      };

      configurePhase = ''
        ./configure --prefix=$out
      ''';

      buildPhase = ''
        make
      '';

      installPhase = ''
        make install
      '';
    }

which will give us the same result as what we had before.

We can chip away at that, verifying that the output is the same every
time we remove one of our phases. Eventually we’ll end up where we
started at the beginning of this post:

    { stdenv, fetchurl }:

    stdenv.mkDerivation rec {
      name = "hello-2.10";

      src = fetchurl {
        url = "mirror://gnu/hello/${name}.tar.gz";
        sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
      };
    }

If you look through the Nixpkgs package set you’ll see that there are a
lot of packages that aren’t much more complicated than this.

The details of the phases and their related settings are in the [Nixpkgs
manual](https://nixos.org/nixpkgs/manual). These settings allow you to
avoid adding your own `configurePhase` just to slightly amend what the
generic build functionality was doing for you:

    { stdenv, fetchurl }:

    stdenv.mkDerivation rec {
      name = "hello-2.10";

      src = fetchurl {
        url = "mirror://gnu/hello/${name}.tar.gz";
        sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
      };
      
      configureFlags = ["--enable-extra-awesomeness"];
    }

(This is not a real configuration flag - don’t do this)

Dealing with dependencies
-------------------------

Nix makes a distinction between build-time and run-time dependencies.

Anything added to the `buildInputs` list of a package will be available
at both build-time and at run-time. This will mean that there will be a
link between the derivation of the package and the derivation of the
dependency, which prevents the garbage collector from cleaning up
dependencies that are required at run-time.

Anything added to the `nativeBuildInputs` list is available only at
build-time.

If we’re build C or C++ projects, Nix will be using the
`NIX_CFLAGS_COMPILE` environment variable to track include directories
of dependencies and the `NIX_LDFLAGS` environment variable to track
library directories of dependencies. In the common cases this is taken
care of for us.

We’re not going to get to play with this using GNU hello, so we’re going
to step things up and package GNU bc.

We can do some prefetching:

    > nix-prefetch-url mirror://gnu/bc/bc-1.07.1.tar.gz

and then write `bc.nix`:

    {stdenv, fetchurl}:

    stdenv.mkDerivation rec {
      name = "bc-1.07.1";
      src = fetchurl {
        url = "mirror://gnu/bc/${name}.tar.gz";
        sha256 = "0amh9ik44jfg66csyvf4zz1l878c4755kjndq9j0270akflgrbb2";
      };
    }

and `default.nix`:

    let
      nixpkgs = import <nixpkgs> {};
    in
      nixpkgs.callPackage ./bc.nix {}

Let’s see how that goes:

    > nix-build
    ...
    ./fix-libmath_h: line 1: ed: command not found
    make[2]: *** [Makefile:632: libmath.h] Error 127
    make[2]: Leaving directory '/tmp/nix-build-bc-1.07.1.drv-0/bc-1.07.1/bc'
    make[1]: *** [Makefile:357: all-recursive] Error 1
    make[1]: Leaving directory '/tmp/nix-build-bc-1.07.1.drv-0/bc-1.07.1'
    make: *** [Makefile:297: all] Error 2
    builder for ‘/nix/store/6v0ikgpdmmkr6cy2gp523lanjd5chwzb-bc-1.07.1.drv’ failed with exit code 2
    error: build of ‘/nix/store/6v0ikgpdmmkr6cy2gp523lanjd5chwzb-bc-1.07.1.drv’ failed

Ouch. We’re missing the standard text editor, which appears to be being
used during build time.

Let’s add that in:

    {stdenv, fetchurl, ed}:

    stdenv.mkDerivation rec {
      name = "bc-1.07.1";
      src = fetchurl {
        url = "mirror://gnu/bc/${name}.tar.gz";
        sha256 = "0amh9ik44jfg66csyvf4zz1l878c4755kjndq9j0270akflgrbb2";
      };
      
      nativeBuildInputs = [ ed ];
    }

and see how much further we get:

    > nix-build
    ...
    /nix/store/1vcp949ka9qnyp6dfv4s9pgjda57vk4x-bash-4.4-p12/bin/bash: line 9: makeinfo: command not found
    make[2]: *** [Makefile:320: bc.info] Error 127
    make[2]: Leaving directory '/tmp/nix-build-bc-1.07.1.drv-0/bc-1.07.1/doc'
    make[1]: *** [Makefile:357: all-recursive] Error 1
    make[1]: Leaving directory '/tmp/nix-build-bc-1.07.1.drv-0/bc-1.07.1'
    make: *** [Makefile:297: all] Error 2
    builder for ‘/nix/store/8rz3nbf74ghpg3xvzs61mdh0xr131q7w-bc-1.07.1.drv’ failed with exit code 2
    error: build of ‘/nix/store/8rz3nbf74ghpg3xvzs61mdh0xr131q7w-bc-1.07.1.drv’ failed

Still no dice.

We can fix that up by adding `texinfo` to our build dependencies:

    {stdenv, fetchurl, ed, texinfo}:

    stdenv.mkDerivation rec {
      name = "bc-1.07.1";
      src = fetchurl {
        url = "mirror://gnu/bc/${name}.tar.gz";
        sha256 = "0amh9ik44jfg66csyvf4zz1l878c4755kjndq9j0270akflgrbb2";
      };
      
      nativeBuildInputs = [ ed texinfo ];
    }

and the result is a great success:

    > nix-build
    /nix/store/6axbha3n5ny261x7wms6ggsnv7p3qzc9-bc-1.07.1

If we were curious while we were packing this up, we might have ducked
into the nix-shell to have a look for any configuration options:

    > nix-shell
    nix-shell > tar zxcf $src
    nix-shell > cd bc-*
    nix-shell > configure --help
    ...
    Optional Packages:
      --with-readline         support fancy command input editing
    ...

We can add that into the mix with the appropriate configure flag and a
run-time dependency:

    {stdenv, fetchurl, ed, texinfo, readline}:

    stdenv.mkDerivation rec {
      name = "bc-1.07.1";
      src = fetchurl {
        url = "mirror://gnu/bc/${name}.tar.gz";
        sha256 = "0amh9ik44jfg66csyvf4zz1l878c4755kjndq9j0270akflgrbb2";
      };

      configureFlags = [ "--with-readline" ];

      nativeBuildInputs = [ ed texinfo ];
      buildInputs = [ readline ];
    }

This will give us an error during the configure step:

    > nix-build
    ...
    Using the readline library.
    configure: error: readline works only with flex.
    builder for ‘/nix/store/pmbdl260i2fbj0q31p36ksk8fs299pg8-bc-1.07.1.drv’ failed with exit code 1
    error: build of ‘/nix/store/pmbdl260i2fbj0q31p36ksk8fs299pg8-bc-1.07.1.drv’ failed

which we can fix by adding `flex` alongside `readline` in our run-time
dependencies:

    {stdenv, fetchurl, ed, texinfo, readline, flex}:

    stdenv.mkDerivation rec {
      name = "bc-1.07.1";
      src = fetchurl {
        url = "mirror://gnu/bc/${name}.tar.gz";
        sha256 = "0amh9ik44jfg66csyvf4zz1l878c4755kjndq9j0270akflgrbb2";
      };

      configureFlags = [ "--with-readline" ];

      nativeBuildInputs = [ ed texinfo ];
      buildInputs = [ readline flex ];
    }

Now we have a successful build with our shiny new build of `bc`:

    > nix-build
    ...
    /nix/store/46cnc5j02pvzpcwnbdfzrzv63p91fk6w-bc-1.07.1

Tidying up
----------

As an aside, we can turn `default.nix` into a function with a default
argument:

    { nixpkgs ? import <nixpgkgs> {} }:

    nixpkgs.callPackage ./hello.nix {}

and it will behave the same way, but we now have the option to use it
with different package sets if we need to.

What about things other than C and C++ ?
----------------------------------------

There are all kinds of other languages and frameworks mentioned in the
Nixpgs manual other than C and C++.

Because this is coming from the QFPL blog, it should be no surprise that
that the next language we’re going to look at is Haskell.


## Acknowledgements
* Original material by Dave Laing on the Queensland Functional Programming Lab's
  websitge as [Building things with Nix].


## 📚 Additional material
* [Nixpkgs manual]


## ⏭️ What's next

For the Melbourne Workshop, we'd like to show an example of [how to nixify a ruby
application] for ease of development.


<!-- in-line links -->
[how to nixify a ruby application]: ./ruby-example/README.md
[Nixpkgs manual]: https://nixos.org/nixpkgs/manual/
[Building things with Nix]: http://qfpl.io/posts/nix/building-things-with-nix/