{-# LANGUAGE UnicodeSyntax #-}
module Ex65_MonadsStatefulComputationsSpec
  ( spec
  ) where

import Control.Exception (evaluate)
import Test.Hspec
import Control.Monad.State as S
import Prelude.Unicode
import Data.Monoid.Unicode
import Control.Monad.Unicode

main :: IO ()
main = hspec spec

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((),a:xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
  ((),newStack1) = push 3 stack
  (a, newStack2) = pop newStack1
  in pop newStack2

{-
    Managing the state by ourselves is tedious.
    Woudln't it be nice to express this, where the state
    is managed for us?

    stackManip = do push 3 a <- pop pop -}
--Enter the State monad:
-- newtype State s a = State { runState ∷ s -> (a, s) }

-- instance Monad (State s) where
--   return x = state $ \s -> (x,s)
--   (State h) >>= f = State $ \s -> let
--                                      (a, newState) = h s
--                                      State g  = f a
--                                  in g newState


pop' :: S.State Stack Int
pop' = state $ \(x:xs)→(x,xs)

push' :: Int -> S.State Stack ()
push' a = state $ \xs → ((),a:xs)

stackManip' :: S.State Stack Int
stackManip' = do
  push' 3
  pop'
  pop'

stackStuff :: S.State Stack ()
stackStuff = do
  a ← pop'
  if a ≡ 5
    then push' 5
    else do
    push' 3
    push' 8

moreStack :: S.State Stack ()
moreStack = do
  a ← stackManip'
  if a ≡ 100
    then stackStuff
    else return ()

spec :: Spec
spec =
    describe "Stateful Computations" $ do
        it "can operate with State on stack" $
             S.runState stackManip' [5,8,2,1] `shouldBe` (5,[8,2,1])
        it "can run conditional logic with Monads"$ do
             S.runState stackStuff [9,0,2,1,0] `shouldBe` ((),[8,3,0,2,1,0])
             S.runState stackStuff [5,0,2,1,0] `shouldBe` ((),[5,0,2,1,0])
        it "can weave other functions with State" $
             S.runState moreStack [5,8,2,1] `shouldBe` ((),[8,2,1])



-- Local Variables:
--   -*- idle-mode: 10; -*-
-- End:
