# Overview

We'll "nixify" a Ruby application; more specifically, a jekyll-based website.
More specifically yet, we'll nixify the Compose :: Conference website.

This exercise can be done under NixOS or, if you have installed Nix on your
Linux or MacOS laptop, you can try it there.

## Type this

Start by cloning the conference site and checking out the appropriate branch:

```
$ git clone https://github.com/candeira/composeconference.git
$ cd composeconference
$ git checkout 2019-melbourne-nix-workshop
```

At this point you may want to take a look around before proceeding. 

The composeconference.org site is just a jekyll site on the gh-pages branch of a
git repo. Its `Gemfile` is simple enough:

```
source 'https://rubygems.org'

gem 'json', '>=1.8.5'
gem 'github-pages'
gem 'jekyll-redirect-from'
```

Let's create a temporary shell containing our bundix and ruby-devEnv
dependencies, use `bundle lock` to generate a `Gemfile.lock` file, and then use
bundix to generate the `gemset.nix` file to convert the ruby-universe
dependencies into nixpkgs dependencies.

Remember `nix-shell` is the command you use to generate a shell that has build
dependencies for something. In this case, not the build dependencies for the
Compose Conference website, but the build dependencies for the "nixification"
process of the website building tools:

```
$ nix-shell -p ruby.devEnv bundix
(...)
[nix-shell]$ bundle lock
(...)
Fetching gem metadata from https://rubygems.org/..
Resolving dependencies....
Writing lockfile to /home/kandinski/data/work/hack/composeconference/Gemfile.lock
[nix-shell]$ bundix Gemfile.lock
[nix-shell]$ exit
```
We now have all we need to install and run jekyll under Nix.

Open a file called `default.nix` and write this in it:

```
with import <nixpkgs> { };                                                  # 1

let jekyll_env = bundlerEnv {                                               # 2
    name = "jekyll_env";
    ruby = ruby_2_5;
    gemfile = ./Gemfile;
    lockfile = ./Gemfile.lock;
    gemset = ./gemset.nix;
  };
in 
  stdenv.mkDerivation {                                                     # 3
    name= "jekyll_env";
    buildInputs = [ jekyll_env ];                                           # 4

    shellHook = ''
      exec ${jekyll_env}/bin/jekyll serve --incremental --watch             # 5 
    '';
  }
```

Let's see what this does:

  1. The opening `with import <nixpkgs> { };` ensures you don't have to write
     `pkgs.bundlerEnv`, `pkgs.ruby_2_5` etc. everywhere.
  1. `bundlerEnv` is a Nix built-in function for creating Bundler environments.
  1. `stdenv.mkDerivation` is the way most packages are set up in Nix.
  1. We have `buildInputs` because that's what jekyll is: the development
     dependency to our website, which is the product.
  1. The `shellHook` will be ran when you enter a shell with `nix-shell`.
     Because `exec` replaces the bash shell that nix-shell was going to start
     with a jekyll process, once you close jekyll with `C-c` the environment
     will be closed as well.

Now we can start the shell. The first time we'll have to wait a bit while it
downloads and configures dependencies. The second time and ongoing, it will be
faster:

```
$ nix-shell
( ... lots of errors, this is a bit embarrassing ... )
                    done in 1.417 seconds.
 Auto-regeneration: enabled for '/home/kandinski/data/work/hack/composeconference'
    Server address: http://127.0.0.1:4000
  Server running... press ctrl-c to stop.

```

You can now edit the site and the dev server will restart each time you save, just as you expect.

## References and Acknowledgements

  - https://nixos.wiki/wiki/Packaging/Ruby Quick reference for packaging Ruby applications from the wiki
  - https://nixos.org/nixpkgs/manual/#sec-language-ruby Official Nix packaging reference for Ruby
  - https://github.com/manveru/bundix Homepage for Bundix
  - https://stesie.github.io/2016/08/nixos-github-pages-env Now outdated after
    the introduction of bundix, but this is where one author learnt the
    `shellHook = ''exec...''` trick.
  
## What's next
