# Monad Transformers Step by Step

Haskell source for the paper 
[Monad Transformers Step by Step](https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf) by Martin Grabmüller, tweaked to work in 2020 and built using [Stack](https://docs.haskellstack.org/en/stable/README/).


## How this repo is organized

- Shared data structureds can be found in [Lib.hs](https://github.com/metadave/monad-transformers-step-by-step/blob/master/src/Lib.hs) 
- Each of the `eval0` .. `eval6` implementations can be found in [./src](https://github.com/metadave/monad-transformers-step-by-step/tree/master/src) under a similarly named file.  
- Examples of each `eval*` function running can be found in the [test](https://github.com/metadave/monad-transformers-step-by-step/blob/master/test/HUnitTests.hs) file.


## Changes

I've made the following changes to the example source in order for it to compile with the "latest" Haskell tools (as of May 2020). 

- In [Lib.hs](https://github.com/metadave/monad-transformers-step-by-step/blob/master/src/Lib.hs), `Eq` was added 
to the `deriving` block of `Exp` and `Value`.

- As [`fail` was removed from Monad](https://downloads.haskell.org/%7Eghc/8.8.1/docs/html/users_guide/8.8.1-notes.html#base-library), I've replaced calls to `fail` with `error`. 

- I added a `MonadFail` instance for `Identity` in [Lib.hs](https://github.com/metadave/monad-transformers-step-by-step/blob/master/src/Lib.hs)
  - more info can be found here https://wiki.haskell.org/MonadFail_Proposal, see the `Adapting old code` section


Without the `MonadFail` instance above, you'll get compilation errors like this:

```
   • No instance for (MonadFail Identity)
        arising from a do statement
        with the failable pattern ‘IntVal i2’
    • In a stmt of a 'do' block: IntVal i2 <- eval2c env e2
      In the expression:
        do IntVal i1 <- eval2c env e1
           IntVal i2 <- eval2c env e2
           return $ IntVal (i1 + i2)
      In an equation for ‘eval2c’:
          eval2c env (Plus e1 e2)
            = do IntVal i1 <- eval2c env e1
                 IntVal i2 <- eval2c env e2
                 return $ IntVal (i1 + i2)
   |
52 |                                  IntVal i2  <- eval2c env e2
   |                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

- `Control.Monad.Except` is used in place of `import Control.Monad.Error`, as `Control.Monad.Error` has been [deprecated](https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Error.html).
  - as the result of this, you'll see `ErrorT` replaced by `ExceptT` in most of the examples.

# Building

```
git clone https://github.com/metadave/monad-transformers-step-by-step.git
cd monad-transformers-step-by-step
stack test
 # - or-
stack ghci
```

## Interactive Examples

```
stack ghci

import qualified Data.Map as Map
eval0 Map.empty exampleExp
-- will display:
IntVal 18

```
# TODO

- Figure out how to test the eval2* functions

# Similar repos

The following repos contain similar implementations, although they don't seem to work in 2020:

- https://github.com/NickAger/monad-transformers-step-by-step
- https://github.com/danbroooks/monad-transformers-step-by-step
