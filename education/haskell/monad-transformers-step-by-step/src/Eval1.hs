module Eval1 where

import           Control.Monad.Except
import           Control.Monad.Identity
import qualified Data.Map               as Map
import           Data.Maybe
import           Lib

-- converted to monadic style
type Eval1 a = Identity a



runEval1 :: Eval1 a -> a
runEval1 = runIdentity


-- `fail` replaced by `error` per https://downloads.haskell.org/~ghc/8.8.1/docs/html/users_guide/8.8.1-notes.html
eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = maybe (error ("undefined variable: " ++ n)) return $ Map.lookup n env
eval1 env (Plus e1 e2) = do
                            IntVal i1 <- eval1 env e1
                            IntVal i2 <- eval1 env e2
                            return $ IntVal (i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do val1 <- eval1 env e1
                           val2 <- eval1 env e2
                           case val1 of
                              FunVal env' n body -> eval1 (Map.insert n val2 env') body


execEval1 = runEval1 (eval1 Map.empty exampleExp)
