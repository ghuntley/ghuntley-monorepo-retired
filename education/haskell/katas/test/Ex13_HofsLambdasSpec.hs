{-# LANGUAGE UnicodeSyntax #-}

module Ex13_HofsLambdasSpec
  ( spec
  ) where

import Test.Hspec
import Prelude.Unicode
import Data.Monoid.Unicode


main :: IO ()
main = hspec spec

flip' f x y = f y x

spec :: Spec
spec = do
    describe "Lambdas" $ do
        it "can express complex mapping logic" $ do
          map (\x -> if even x then "even" else "odd") [1..4]
                 `shouldBe` ["odd", "even", "odd", "even"]
        it "can take 2 arguments - like normal functions" $ do
            {- use zipWith here -}
            {- The calculation logic should be: (x * 30 + 3) / y) -}
            zipWith (\x y ->  (x ⋅ 30 + 3) / y) [1,2,3] [4,5,6]
              `shouldBe` [8.25,12.6,15.5]
        it "can pattern match in lambdas" $ do
            {- use map here -}
            map (\(x,y) -> x + y) [(1,2),(3,4),(5,6)]
              `shouldBe` [3,7,11]
        it "can make flip more expressive" $ do
            {- create a new function that uses lambda -}
             flip' (-) 5 3 `shouldBe` -2
