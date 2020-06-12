{-# LANGUAGE UnicodeSyntax #-}

module Ex43_FaNewTypeSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Control.Applicative
import Prelude.Unicode
import Data.Monoid.Unicode
import Data.Map.Unicode
import Data.Sequence.Unicode
import Control.Applicative.Unicode


main :: IO ()
main = hspec spec

{-
    This would work:
    data ZipList a = ZipList { getZipList :: [a] }

    But this is better:
    newtype ZipList a = ZipList { getZipList :: [a] }
    * faster (no boxing/unboxing)
    * it can have only one value constructor

    Making a tuple to be an instance of a Functor is not possible.
    We can newtype the tuple in a way that the second type
    parameter represents the type of the first component of the tuple.
-}

newtype CharList = CharList { getCharList ∷ [Char] } deriving (Eq, Show)

newtype Pair b a = Pair { getPair ∷ (a,b)}

instance Functor (Pair c) where
     fmap f (Pair (a, b)) = Pair (f a, b)

spec :: Spec
spec = do
    describe "newtype" $ do
        it "can print values" $ do
             let charList = CharList "this will be shown!"
             show charList
                 `shouldBe` "CharList {getCharList = \"this will be shown!\"}"
        it "can equate values" $ do
             CharList "benny" == CharList "benny"
                 `shouldBe` True
             CharList "benny" == CharList "oisters"
                 `shouldBe` False
        it "works with the newtype Pair" $ do
             (getPair $ fmap (*100) (Pair (2,3)))
                 `shouldBe` (200, 3)
             (getPair $ fmap reverse (Pair ("london calling", 3)))
                 `shouldBe` ("gnillac nodnol", 3)
