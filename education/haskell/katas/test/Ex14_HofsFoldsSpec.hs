{-# LANGUAGE UnicodeSyntax #-}

module Ex14_HofsFoldsSpec
  ( spec
  ) where

import Test.Hspec
import Prelude.Unicode
import Data.Monoid.Unicode

main :: IO ()
main = hspec spec

sum' (x:xs) = foldl (\acc x -> acc+x) 0 (x:xs)

elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map' f xs = foldl (\acc x -> acc ++ [f x] ) [] xs

maximum' (x:xs) = foldl (\acc x -> max acc x) x xs

reverse' (x:xs) = foldl (\acc x -> x : acc) [] (x:xs)

product' (x:xs) = foldl (\acc x -> acc*x) 1 (x:xs)

filter' p (x:xs) = foldr (\x acc -> if p x then x : acc else acc) [] (x:xs)

last' (x:xs) = foldl (\acc x -> x) x xs

head' (x:xs) = foldr (\x acc -> x) x (x:xs)

spec :: Spec
spec = do
    describe "Folds" $ do
        it "can sum up a list with foldl" $ do
             sum' [1,2,3,4] `shouldBe` 10
        it "can check if an item is in a list" $ do
             elem' 3 [1,2,3,4] `shouldBe` True
             elem' 5 [1,2,3,4] `shouldBe` False
        it "can map over values with foldr" $ do
             map' (*2) [1,2,3] `shouldBe` [2,4,6]
        it "can find the maximum in a list" $ do
             maximum' [1,2,3] `shouldBe` 3
        it "can reverse a list with foldl" $ do
             reverse' [1,3,5] `shouldBe` [5,3,1]
        it "can calculate a product of a list" $ do
             product' [2,2,3] `shouldBe` 12
        it "can filter a list with foldr" $ do
             filter' (<3) [1,2,3,5] `shouldBe` [1,2]
        it "can pick the head of the list" $ do
             head' [5,4,3] `shouldBe` 5
        it "can pick the last item of the list" $ do
             last' [5,4,3] `shouldBe` 3
