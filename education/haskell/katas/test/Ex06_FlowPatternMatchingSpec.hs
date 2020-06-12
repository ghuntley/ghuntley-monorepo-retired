module Ex06_FlowPatternMatchingSpec
  ( spec
  ) where

import Test.Hspec
import Test.Hspec
import Test.QuickCheck
import Prelude.Unicode
import Data.Monoid.Unicode

factorial :: Int -> Int
factorial x
  | x <= 1     = 1
  | otherwise = x * factorial (x-1)


charName 'a' = "Albert"


addVectors (a,b) (c,d) = (a+c, b+d)

first (x,_,_) = x

second (_,y,_) = y

third (_,_,z) = z


head' (x:xs) = x

tell :: [Int] -> String
tell [] = "This list is empty"
tell [x] = "This list has one element: " ++ show x
tell (x:y:[]) = "This list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:xs) = "This list is too long"


length' [] = 0
length' (x:xs) = 1 + length' xs


sum' [] = 0
sum' (x:xs) = x + sum' xs


firstLetter [] = "Empty string, whoops!"
firstLetter l@(x:xs) = "The first letter of " ++ l ++ " is " ++ [x]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Pattern matching" $ do
        it "can be used in factorial calc" $ do
              factorial 5 `shouldBe` 120
        it "can fail when no default case" $ do
             charName 'a' `shouldBe` "Albert"
             -- charName'd' `shouldThrow` PatternMatchFail
        it "can be used on tuples" $ do
             addVectors (1,2)(3,4) `shouldBe` (4,6)
        it "can be used on triples" $ do
             first (1,2,3) `shouldBe` 1
             second (1,2,3) `shouldBe` 2
             third (1,2,3) `shouldBe` 3
        it "can pattern list comprehensions" $ do
             let xs = [(1,3),(4,3),(2,4),(5,3),(5,6),(3,1)]
             [a+b | (a,b) <- xs] `shouldBe` [4,7,6,8,11,4]
        it "can be used for the head function" $ do
             head' [2,3,4] `shouldBe` 2
             head' "Hello" `shouldBe` 'H'
        it "can safely process a list" $ do
             tell [] `shouldBe` "This list is empty"
             tell [1] `shouldBe` "This list has one element: 1"
             tell [1,2] `shouldBe` "This list has two elements: 1 and 2"
             tell [1,2,3] `shouldBe` "This list is too long"
        it "can count elements in list with recursion" $ do
             length' [] `shouldBe` 0
             length' [1,2,3] `shouldBe` 3
        it "can reduce add a list" $ do
             sum' [] `shouldBe` 0
             sum' [1,2,3] `shouldBe` 6
        it "can hold the original item with pattern" $ do
             firstLetter "" `shouldBe` "Empty string, whoops!"
             firstLetter "Dracula" `shouldBe` "The first letter of Dracula is D"
