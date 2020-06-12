module Eval5 where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Function
import qualified Data.Map               as Map
import           Data.Maybe

import           Lib


type Eval5 a = ReaderT Env  (ExceptT String
                                (WriterT [String] (StateT Integer Identity))) a


-- MOVE TO LIB
tick :: (Num s, MonadState s m) => m ()
tick = do  st <- get
           put (st + 1)


runEval5            ::  Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env st ev  =
    runIdentity (runStateT (runWriterT (runExceptT (runReaderT ev env))) st)

eval5               ::  Exp -> Eval5 Value
eval5 (Lit i)       =   do  tick
                            return $ IntVal i
eval5 (Var n)       =   do  tick
                            tell [n]
                            env <- ask
                            case Map.lookup n env of
                               Nothing -> throwError ("unbound variable: " ++ n)
                               Just val -> return val
eval5 (Plus e1 e2)  =   do  tick
                            e1'  <- eval5 e1
                            e2'  <- eval5 e2
                            case (e1', e2') of
                              (IntVal i1, IntVal i2) ->
                                  return $ IntVal (i1 + i2)
                              _ -> throwError "type error in addition"
eval5 (Abs n e)     =   do  tick
                            env <- ask
                            return $ FunVal env n e
eval5 (App e1 e2)   =   do  tick
                            val1  <- eval5 e1
                            val2  <- eval5 e2
                            case val1 of
                               FunVal env' n body ->
                                  local (const (Map.insert n val2 env'))
                                    (eval5 body)
                               _ -> throwError "type error in application"
