module Lib where

import qualified Data.Map as Map
import qualified Control.Monad.Fail     as Fail
import           Control.Monad.Identity

type Name = String

data Exp
    = Lit Integer
    | Var Name
    | Plus Exp
           Exp
    | Abs Name
          Exp
    | App Exp
          Exp
    deriving (Show, Eq)

data Value
    = IntVal Integer
    | FunVal Env
             Name
             Exp
    deriving (Show, Eq)

type Env = Map.Map Name Value


-- https://wiki.haskell.org/MonadFail_Proposal
-- see the `Adapting old code` section
instance Fail.MonadFail Identity where
  fail = Fail.fail

{-|
Without the MonadFail instance above, you'll get compilation errors like this:

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
-}

exampleExp :: Exp
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))