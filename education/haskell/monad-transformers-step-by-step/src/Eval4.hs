module Eval4 where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.Map               as Map
import           Data.Maybe

import           Lib

type Eval4 a = ReaderT Env (ExceptT String (StateT Integer Identity)) a

runEval4            ::  Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st ev  =   runIdentity (runStateT (runExceptT (runReaderT ev env)) st)

tick :: (Num s, MonadState s m) => m ()
tick = do  st <- get
           put (st + 1)


eval4               ::  Exp -> Eval4 Value
eval4 (Lit i)       =   do  tick
                            return $ IntVal i
eval4 (Var n)       =   do  tick
                            env <- ask
                            case Map.lookup n env of
                               Nothing -> throwError ("unbound variable: " ++ n)
                               Just val -> return val
eval4 (Plus e1 e2)  =   do  tick
                            e1'  <- eval4 e1
                            e2'  <- eval4 e2
                            case (e1', e2') of
                              (IntVal i1, IntVal i2) ->
                                  return $ IntVal (i1 + i2)
                              _ -> throwError "type error in addition"
eval4 (Abs n e)     =   do  tick
                            env <- ask
                            return $ FunVal env n e
eval4 (App e1 e2)   =   do  tick
                            val1  <- eval4 e1
                            val2  <- eval4 e2
                            case val1 of
                               FunVal env' n body ->
                                  local (const (Map.insert n val2 env'))
                                    (eval4 body)
                               _ -> throwError "type error in application"
