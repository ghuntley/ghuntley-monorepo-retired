import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Monoid
import qualified Data.Foldable as F

-- Define a tree Data Type

-- Add implementation for F.Foldable with F.foldMap to Tree
{- instance F.Foldable Tree where -}
    {- ___ -}

{-
-- Create an sample tree sructure
testTree = Node 5
            (Node 3
                (Node 1 Empty Empty)
                (Node 6 Empty Empty)
            )
            (Node 9
                (Node 8 Empty Empty)
                (Node 10 Empty Empty)
            )
-}

main :: IO ()
main = hspec $ do
    describe "Foldable" $ do
        it "Foldable foldr and Prelude Foldr are the same for lists" $ do
            pending
            {- foldr (___) 1 [1,2,3] `shouldBe` 6 -}
            {- F.foldr (___) 1 [1,2,3] `shouldBe` 6 -}
        it "works with Maybe" $ do
            pending
            {- F.foldl (___) 2 (Just ___) `shouldBe` 11 -}
            {- F.foldl (___) False (Just ___) `shouldBe` True -}
        it "can fold a tree with Foldable implementation" $ do
            pending
            {- F.foldl ___ testTree `shouldBe` 42    -- sum -}
            {- F.foldl ___ testTree `shouldBe` 64800 -- product -}
        it "can tell if the tree contains a single value" $ do
            pending
            {- Use Any to tell if 3 is in the tree or not -}
            {- ___ -}
                {- `shouldBe` True -}
        it "applies the function to all elements in the Tree" $ do
            pending
            {- Use Any to tell if any item is >15 in the Tree -}
            {- ___ -}
                {- `shouldBe` False -}
        it "can turn the Tree into a list" $ do
            pending
            {- ___ testTree `shouldBe` [1,3,6,5,8,9,10] -}
