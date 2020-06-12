{-# LANGUAGE UnicodeSyntax #-}

module Ex16_HofsFunctionApplicationSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Prelude.Unicode
import Data.Monoid.Unicode

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Function application with $" $ do
        it "let's get rid of parens" $ do
          {- double the list of 1-6 and sum its values -}
          (sum $ map (*2) [1..6])  `shouldBe` 42
          {- sqrt of 3 + 4 + 9 -}
          (sqrt $ sum [3,4,9]) `shouldBe` 4
          {- double 1-5, sum of elements greater than 5 -}
          (sum $ filter (>5) $ map (*2) [1..5]) `shouldBe` 24
