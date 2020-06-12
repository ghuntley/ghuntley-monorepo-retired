{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ex47_FaNewTypeMonoidSpec
  ( spec
  ) where

import Data.Monoid
import Test.Hspec
import Data.Monoid.Unicode
import Prelude.Unicode
import Data.Monoid.Unicode
import Data.Map.Unicode
import Data.Sequence.Unicode
import Control.Applicative.Unicode


main :: IO ()
main = hspec spec

newtype AInt = A { unA :: Int } deriving (Show, Eq, Num)


instance Semigroup AInt where
  (<>) = (+)
  
-- monoid under addition
instance Monoid AInt where
  mempty = 0
  mappend = (+)

newtype MInt = M { unM :: Int } deriving (Show, Eq, Num)

instance Semigroup MInt where
  (<>) = (*)

-- monoid under multiplication
instance Monoid MInt where
  mempty = 1
  mappend = (*)

spec :: Spec
spec =
    describe "New Type Monoids" $ do
        it "can be used to create distinct types" $ do
             unM (2 `mappend` 5 :: MInt)
                 `shouldBe` 10
             unA (2 `mappend` 5 :: AInt)
                 `shouldBe` 7
