{-# LANGUAGE UnicodeSyntax #-}

module Ex10_FlowCaseExpressionSpec
  ( spec
  ) where

import Test.Hspec
import Prelude.Unicode
import Data.Monoid.Unicode

main :: IO ()
main = hspec spec
    {- ___ -}
    {- ___ -}
    {- where ___ [] = "is empty." -}
          {- ___ -}
          {- ___ -}


head' :: [a] -> a
head' xs
  | null xs = error "No first item"
  | otherwise = head xs

describeList :: [a] -> String
describeList xs
  | null xs =  "The list is empty."
  | length xs ≡ 1 = "The list is a singleton list."
  | otherwise = "The list is a longer list."

 -- Case statement can be written with patten matching
describeList xs = "The list is " ++ what xs
     where what [] = "is empty."
           what ([x]) = "a singleton list."
           what (x:_) = "a longer list."

-- head' :: [a] -> a
-- head' xs = case xs of ...
-- describeList :: [a] -> String
-- describeList xs = "The list is " ++ case xs ___
-- Case statement can be written with patten matching
-- describeList xs = "The list is " ++ what xs

spec :: Spec
spec =
    describe "Case expressions" $ do
        it "can be used anywhere" $ do
             head' [1,3] `shouldBe` 1
        it "can be even used in expressions" $ do
             describeList [] `shouldBe` "The list is empty."
             describeList [1] `shouldBe` "The list is a singleton list."
             describeList [1,2] `shouldBe` "The list is a longer list."
