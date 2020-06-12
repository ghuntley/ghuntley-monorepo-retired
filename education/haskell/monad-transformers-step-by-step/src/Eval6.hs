module Eval6 where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Function
import qualified Data.Map               as Map
import           Data.Maybe

import           Lib

type Eval a = ReaderT Env (ExceptT String
                          (WriterT [String] (StateT Integer IO))) a

runEval6 :: Env -> Integer -> Eval a -> IO ((Either String a, [String]), Integer)
runEval6 env st ev =
    runStateT (runWriterT $ runExceptT $ runReaderT ev env) st

tick :: (Num s, MonadState s m) => m ()
tick = do st <- get
          put (st + 1)

eval6 :: Exp -> Eval Value
eval6 (Lit i) = do
    tick
    liftIO $ print i
    return $ IntVal i
eval6 (Var n) = do
    tick
    tell [n]
    env <- ask
    case Map.lookup n env of
        Just n' -> return n'
        Nothing -> throwError ("undefined variable: " ++ n)
eval6 (Plus a b) = do
    tick
    ia <- eval6 a
    ib <- eval6 b
    case (ia, ib) of
        (IntVal a', IntVal b') -> return $ IntVal (a' + b')
        _                      -> throwError "type error in addition"
eval6 (Abs n e) = do
    tick
    env <- ask
    return $ FunVal env n e
eval6 (App a b) = do
    tick
    a' <- eval6 a
    b' <- eval6 b
    case a' of
      FunVal env' n body -> local (const $ Map.insert n b' env') (eval6 body)
      _                  -> throwError "type error in application"
