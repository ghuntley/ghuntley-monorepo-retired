{-# LANGUAGE UnicodeSyntax #-}

module Ex26_TypesTypeParametersSpec
  ( spec
  ) where

import Test.Hspec
import Test.QuickCheck
import Data.List.Unicode
import Prelude.Unicode
import Data.Monoid.Unicode




main :: IO ()
main = hspec spec

{-
    Create a Vector type constractor,
    Data constructor should take in 3 arguments
    Type could be displayed and eq
-}
data Vector a = Vector a a a deriving (Show, Eq)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector x1 y1 z1) (Vector x2 y2 z2) = (Vector (x1+x2) (y1+y2) (z1+z2))

scalarMult  :: (Num t) => Vector t -> t -> Vector t
scalarMult (Vector x y z) l = (Vector (l*x) (l*y) (l*z))

vectMult :: (Num t) => Vector t -> Vector t -> t
vectMult (Vector x y z)  (Vector i j k) =  (i*x)+(j*y)+(k*z)

spec :: Spec
spec = do
    describe "Type parameters" $ do
        it "can add two vectors together" $ do
             Vector 3 5 8 `vplus` Vector 9 2 8
                 `shouldBe` Vector 12 7 16
        it "can add three vectors together" $ do
             Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3
                 `shouldBe` Vector 12 9 19
        it "can multiply a vector with a scalar" $ do
             Vector 3 5 8 `scalarMult` 10
                 `shouldBe` Vector 30 50 80
        it "can multiply two vectors" $ do
             Vector 3 5 8 `vectMult` Vector 2 3 4
                 `shouldBe` 6+15+32
