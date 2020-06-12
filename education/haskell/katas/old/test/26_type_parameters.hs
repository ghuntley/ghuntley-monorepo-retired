import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

{-
    Create a Vector type constractor,
    Data constructor should take in 3 arguments
    Type could be displayed and eq
-}

{- vplus :: (Num t) => Vector t -> Vector t -> Vector t -}

{- vectMult :: (Num t) => Vector t -> t -> Vector t -}

{- scalarMult :: (Num t) => Vector t -> Vector t -> t -}

main :: IO()
main = hspec $ do
    describe "Type parameters" $ do
        it "can add two vectors together" $ do
            pending
            {- Vector 3 5 8 `vplus` Vector 9 2 8 -}
                {- `shouldBe` Vector 12 7 16 -}
        it "can add three vectors together" $ do
            pending
            {- Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3 -}
                {- `shouldBe` Vector 12 9 19 -}
        it "can multiply a vector with a scalar" $ do
            pending
            {- Vector 3 5 8 `vectMult` 10 -}
                {- `shouldBe` Vector 30 50 80 -}
        it "can multiply two vectors" $ do
            pending
            {- Vector 3 5 8 `scalarMult` Vector 2 3 4 -}
                {- `shouldBe` 6+15+32 -}
