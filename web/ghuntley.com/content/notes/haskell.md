---
title: haskell
layout: notes
---

# ghci

GHCi tip: `import Module` and `:load Module` do very different things! 

`import` brings the exports of `Module` into scope. 

`:load` puts you in the context of that module, including language extensions, everything imported in that module, and non-exported terms from the module


# modelling errors as data types 

http://lambdafoo.com/blog/2018/06/22/transformers-either/

# pattern matching

When you see a constructor on the left hand side that's pattern matching
```haskell
k :: Z -> String
k (X s) = s
```

Alternatively you can use `case of`

```haskell
l :: Z -> String
l Z = case Z of
  X s -> s
  Y -> "y"
```

# the tick convention
similar to `klass` in Java. Used to escape variable bindings

```haskell
data Bool` = True` | False`
```

# polymorphism

Polmrophic types in haskell always start with a lower case letter. A lower case type means polymorphic [generic]

```haskell
i :: a -> a
i x = x
```


{{< tweet 996628203463569410 >}}


https://qnikst.github.io/posts/2018-10-29-metrics-haskell.html

# profiling
http://neilmitchell.blogspot.com/2018/10/announcing-profiterole-ghc-profile.html?m=1

# Handling Failure
* https://hackage.haskell.org/package/transformers-either
* https://github.com/expression-oriented/failing-gracefully
* https://hackage.haskell.org/package/hoist-error
* https://github.com/alephcloud/hs-hoist-error

# Maintainability of Haskell Code
* https://abailly.github.io/posts/haskell-reliability.html
* http://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html

# structured logging
- https://github.com/Soostone/katip/blob/master/katip/README.md

# Event Sourcing
* https://github.com/abailly/hevents
* https://abailly.github.io/posts/event-source.html

# Input Validation
* https://github.com/qfpl/validation

# Configuration Management
* https://propellor.branchable.com/

# Infrastructure and DevOps Tooling
* https://abailly.github.io/posts/cm-infra-1.html
* https://abailly.github.io/posts/cm-infra-2.html
* https://propellor.branchable.com/

# Website Application Architecture
* https://abailly.github.io/posts/cm-arch-design.html

# Software Transactional Memory
* http://blog.akii.de/posts/2018-02-19-geoip-lookup-part-1.html
* https://abailly.github.io/posts/note-on-stm.html

# Concurrent Programming
* https://simonmar.github.io/pages/pcph.html

# GHC Internals
* http://www.scs.stanford.edu/11au-cs240h/notes/ghc.html

# Recommended Reading
* https://github.com/bfpg/cis194-yorgey-lectures

# Isomorphic Haskell
* https://github.com/dmjio/miso/blob/master/README.md

# Domain Driven Design
* https://abailly.github.io/posts/cm-arch-design.html
* http://blog.akii.de/posts/2017-06-04-eventsourcing-in-haskell.html
* https://abailly.github.io/posts/dependent-types-ddd.html

# HTTP Clients
* http://hackage.haskell.org/package/http-client-restricted-0.0.1/docs/Network-HTTP-Client-Restricted.html

# Existential Types
* https://abailly.github.io/posts/existential-types.html

applications.md
# Full Websites
* https://github.com/ahushh/Monaba

# Webserver and API
* https://github.com/yamadapc/yesod-servant-example
* https://haskell-servant.github.io/posts/2016-02-06-servant-swagger.html
* http://www.parsonsmatt.org/2016/12/18/servant_in_yesod_-_yo_dawg.html
c9.md

     ,-----.,--.                  ,--. ,---.   ,--.,------.  ,------.
    '  .--./|  | ,---. ,--.,--. ,-|  || o   \  |  ||  .-.  \ |  .---'
    |  |    |  || .-. ||  ||  |' .-. |`..'  |  |  ||  |  \  :|  `--, 
    '  '--'\|  |' '-' ''  ''  '\ `-' | .'  /   |  ||  '--'  /|  `---.
     `-----'`--' `---'  `----'  `---'  `--'    `--'`-------' `------'
    ----------------------------------------------------------------- 


Setting up `haskell-vim-now` on a Cloud9 workspace:

  1. Start with a _Blank_ workspace.
  2. Increase resources (use at least `7GB` for disk and `2GB` for ram, but more is better).
  3. Edit `styles.css` to use a powerline font:
```css
@import url(https://cdn.rawgit.com/wernight/powerline-web-fonts/f3821a36beeba53e6e937319d4ee636ef30a352c/PowerlineFonts.css);
```
  4. Update `apt`:
```sh
sudo apt-get update
```
  5. Install the Haskell stack:
```sh
curl -sSL https://get.haskellstack.org/ | sh
```
  6. Install `haskell-vim-now`:
```sh
curl -L https://git.io/haskell-vim-now > /tmp/haskell-vim-now.sh
bash /tmp/haskell-vim-now.sh
```
  7. Enjoy!

### Optional

Switch to a newer version of `node` with `nvm`:
```sh
nvm install 6.9.2
nvm alias default 6.9.2
```

Uninstall any older versions:
```sh
nvm ls
nvm uninstall 4.6.1
```
data61.md

Execute `nix-shell` to bootstrap the environment

Create `hello.hs`

```hs
x :: Integer
x = 3
```


```
ghci -ignore-dot-ghci 

:load hello.hs #loads from filesystem
:r # reload loaded modules (inc hello.hs)
:t x # obtain type signature of x
```

# Lists

cons :: https://en.wikipedia.org/wiki/Cons

cons - head and tail

```hs
>> :t (:.)
(:.) :: t -> List t -> List t
```

```hs
>> 1 :. 2 :. 3 :. 4 :. Nil
[1,2,3,4]

```

The word "cons" and expressions like "to cons onto" are also part of a more general functional programming jargon. Sometimes operators that have a similar purpose, especially in the context of list processing, are pronounced "cons". (A good example is the :: operator in ML, Scala, F# and Elm or the : operator in Haskell, which adds an element to the beginning of a list.)

# Rosetta

generics :: type variables
cons :: https://en.wikipedia.org/wiki/Cons

cons - head and tail
```
>> :t (:.)
(:.) :: t -> List t -> List t
```


# Pattern Matching

Can enable warnings when not all patterns are matched ```set fwarn-incomplete-patterns``` see https://stackoverflow.com/questions/31866379/haskell-non-exhaustive-pattern-matching-in-haskell

# Resources

### Lists

`const`

https://en.wikipedia.org/wiki/Unary_function
https://hackage.haskell.org/package/base-4.10.1.0/docs/Prelude.html#v:const

### Point Free

* https://wiki.haskell.org/Pointfree
* http://pointfree.io/

``` sum' xs = foldr (+) 0 xs```

# Workflow
Look at the signature and write down what you have (tm) and write down what you need to get to (tm)                                                                       

```hs
data Optional a =
  Full a
  | Empty
  deriving (Eq, Show)
```

```
What I have                                                                                    
----
x :: a
f :: a -> Optional b
----
What do I need to get to?
? :: Optional b
```

# People
* David
* Tim


# Day 1
- `src/Optional.hs`
- `src/List.hs`


# Determining file location of a type

```hs
>> :i Full
data Optional a = Full a | ...
        -- Defined at src/Course/Optional.hs:15:3
```
education.md
# Videos
* https://channel9.msdn.com/Series/C9-Lectures-Erik-Meijer-Functional-Programming-Fundamentals/Lecture-Series-Erik-Meijer-Functional-Programming-Fundamentals-Chapter-1
index.md

# GC

https://tech-blog.capital-match.com/posts/2-cpu-credits.html

https://github.com/ezyang/compact/blob/master/README.md



# Patterns
https://www.fpcomplete.com/blog/2017/06/readert-design-pattern


# date time

https://hackage.haskell.org/package/auto-update-0.1.4/docs/Control-AutoUpdate.html

# hot paths
https://www.yesodweb.com/blog/2014/08/announcing-auto-update

# compilation
https://rybczak.net/2016/03/26/how-to-reduce-compilation-times-of-haskell-projects/

packages.md
# Caching

https://github.com/SumAll/haskell-cached-io
README.md
http://blog.wuzzeb.org/full-stack-web-haskell/
tooling.md
# IDE
* Comparision of IDE's - https://github.com/rainbyte/haskell-ide-chart

# Misc
https://begriffs.com/posts/2015-07-27-haskell-source-navigation.html
vim.md.
https://github.com/begriffs/haskell-vim-now

https://gist.github.com/gatlin/7754289

# GHC Internals
* http://www.scs.stanford.edu/11au-cs240h/notes/ghc.html
