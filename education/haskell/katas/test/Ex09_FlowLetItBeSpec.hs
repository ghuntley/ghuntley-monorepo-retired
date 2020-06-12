{-# LANGUAGE UnicodeSyntax #-}

module Ex09_FlowLetItBeSpec
  ( spec
  ) where

import Test.Hspec
import Prelude.Unicode
import Data.Monoid.Unicode

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let
    sideArea = 2 * pi ⋅ r ⋅ h
    topArea = π ⋅ r ⋅ r
  in sideArea + 2 * topArea

cylinder' :: (RealFloat a) => a -> a -> a
cylinder' r h = sideArea + 2 * topArea
  where
    sideArea = 2 * π ⋅ r ⋅ h
    topArea = π ⋅ r ⋅ r


calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi | (x, y) ← xs, let bmi = x / (y * y), bmi ≥ 25.0]

main :: IO ()
main = hspec spec
    {- ... -}
    {- ... -}
    {- in sideArea + 2 * topArea -}
    {- where ... -}
          {- ... -}

-- cylinder :: (RealFloat a) => a -> a -> a
-- cylinder r h =
-- cylinder' :: (RealFloat a) => a -> a -> a
-- cylinder' r h = sideArea + 2 * topArea
-- calcBmis :: (RealFloat a) => [(a,a)] -> [a]

spec :: Spec
spec =
    describe "Let bindings are almost like where" $ do
        it "can calculate cylinder surface area" $ do
           cylinder 1 2 `shouldBe` 18.84955592153876
           cylinder' 1 2 `shouldBe` 18.84955592153876
        it "is an expression in itself" $ do
           let result = [let square x = x ⋅ x in (square 5,square 3,square 2)]
           result `shouldBe` [(25,9,4)]
        it "can be used in list comprehensions" $ do
             calcBmis [(80,1.9)] `shouldBe` []
