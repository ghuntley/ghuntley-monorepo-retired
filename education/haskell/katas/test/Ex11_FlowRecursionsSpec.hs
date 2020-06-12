module Ex11_FlowRecursionsSpec
  ( spec
  ) where

import Test.Hspec
import Prelude.Unicode
import Data.Monoid.Unicode


maximum' (x:xs)
  | null xs = x
  | x ≥ maximum' (xs) = x
  | otherwise = maximum' (xs)

-- replicate' ∷ Int → a → [a]
replicate' n x
  | n ≡ 1 = [x]
  | otherwise = x:replicate' (n-1) x

take' n (x:xs)
  | n≡1 = [x]
  | otherwise = x:take' (n-1) xs

reverse' (x:xs)
  | null xs = [x]
  | otherwise = (reverse' xs) ++ [x]


repeat' x = x: repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

myElem n (x:xs)
  | n ≡ x = True
  | null xs = False
  | otherwise  = myElem n xs

quicksort [] = []
quicksort (x:xs) = quicksort small ++ (x : quicksort large)
   where small = [y | y <- xs, y <= x]
         large = [y | y <- xs, y > x]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Recursion" $ do
        it "calculates maximum" $ do
             maximum' [2,5,1] `shouldBe` 5
        it "replicates items" $ do
             replicate' 5 'a' `shouldBe` "aaaaa"
        it "takes from a collection" $ do
             take' 3 "abcde" `shouldBe` "abc"
        it "reverses a collection" $ do
             reverse' [1,2,3] `shouldBe` [3,2,1]
        it "can repeat items" $ do
             take' 3 (repeat' 'a') `shouldBe` "aaa"
        it "can zip items" $ do
             zip' [1,2,3] ['a','b'] `shouldBe` [(1,'a'),(2,'b')]
        it "can check if an item is an element of a list" $ do
             myElem 3 [1,2,3] `shouldBe` True
        it "can do QuickSort - easily" $ do
             quicksort [3,1,2] `shouldBe` [1,2,3]
             quicksort "attila" `shouldBe` "aailtt"
