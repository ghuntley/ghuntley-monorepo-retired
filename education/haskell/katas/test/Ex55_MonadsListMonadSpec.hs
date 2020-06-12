{-# LANGUAGE UnicodeSyntax #-}

module Ex55_MonadsListMonadSpec
  ( spec
  ) where

import Test.Hspec
import Prelude.Unicode
import Data.Monoid.Unicode
import Control.Monad.Unicode

main :: IO ()
main = hspec spec

{-
    This is what Monad instance for lists looks like:

    instance Monad [] where
        return x = [x]
        xs >>= f = concat (map f xs)
        fail _ = []
-}

-- Use the do notation for chars and ints
listOfTuples :: [(Int, Char)]
listOfTuples = do
  myInt ← [1..2]
  myChar ← ['a'..'b']
  return (myInt,myChar)

spec :: Spec
spec = do
    describe "List monad" $ do
        it "can leverage do notation" $ do
            -- use a lambda here
             ([3,4,5] >>= \x → [x, -x])
                 `shouldBe` [3,-3,4,-4,5,-5]
             ([] >>= \x -> ["bad","mad","rad"])
                 `shouldBe` []
             length ([1,2,3] >>= \x -> [])
                 `shouldBe` 0
        it "can create a list of tuples by chaining" $ do
             let ts = [1,2] >>= \n -> ['a','b'] >>= \ch → return (n,ch)
             let result = [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
             ts `shouldBe` result
             listOfTuples `shouldBe` result
             [(n,ch) | n <- [1..2], ch <- ['a','b']]
                 `shouldBe` result
