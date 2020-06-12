module Eval3 where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import qualified Data.Map               as Map
import           Data.Maybe

import           Lib


type Eval3 a = ReaderT Env (ExceptT String Identity) a

runEval3     :: Env -> Eval3 a -> Either String a
runEval3 env ev  = runIdentity (runExceptT (runReaderT ev env))

eval3               ::  Exp -> Eval3 Value
eval3 (Lit i)       =   return $ IntVal i
eval3 (Var n)       =   do  env <- ask
                            case Map.lookup n env of
                               Nothing -> throwError ("unbound variable: " ++ n)
                               Just val -> return val
eval3 (Plus e1 e2)  =   do  e1'  <- eval3 e1
                            e2'  <- eval3 e2
                            case (e1', e2') of
                              (IntVal i1, IntVal i2) ->
                                  return $ IntVal (i1 + i2)
                              _ -> throwError "type error in addition"
eval3 (Abs n e)     =   do  env <- ask
                            return $ FunVal env n e
eval3 (App e1 e2)   =   do  val1  <- eval3 e1
                            val2  <- eval3 e2
                            case val1 of
                               FunVal env' n body ->
                                  local (const (Map.insert n val2 env'))
                                    (eval3 body)
                               _ -> throwError "type error in application"
