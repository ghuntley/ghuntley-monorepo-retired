{-# LANGUAGE UnicodeSyntax #-}

module Ex19_ModulesListFunctionsSpec
  ( spec
  ) where


import Test.Hspec
import Test.QuickCheck
import qualified Data.List as L
import Data.List.Unicode
import Data.Function
import Prelude.Unicode
import Data.Monoid.Unicode

main :: IO ()
main = hspec spec

{- search :: (Eq a) => [a] -> [a] -> Bool -}
{- search needle haystack = -}
    {- let nlen = length needle -}
    {- in foldl (\acc x -> if take nlen x == needle then True else acc) False (L.tails haystack) -}

spec :: Spec
spec = do
    describe "More List functions" $ do
        it "can iterate as an infinite list" $ do
             (take 10 $ iterate (*2) 1) `shouldBe` [1,2,4,8,16,32,64,128,256,512]
             (take 3 $ iterate (++ "haha") "haha")
                 `shouldBe` ["haha", "hahahaha", "hahahahahaha"]
        it "can split a list at the position specified" $ do
             (splitAt 3 "hey man") `shouldBe` ( "hey", " man")
             (splitAt 100 "hey man") `shouldBe` ("hey man", "")
             (splitAt 0  "hey man") `shouldBe` ("", "hey man")
        it "can exhaust a list with inits and tails" $ do
            {- hint: use `inits` and `tails` -}
             L.inits "woot" `shouldBe` ["","w","wo","woo","woot"]
             L.tails "woot" `shouldBe` ["woot", "oot", "ot", "t", ""]
        it "can tell if a string starts with another one" $ do
            {- hint: use `isPrefixOf` -}
             L.isPrefixOf "hey" "hey there!" `shouldBe` True
             L.isPrefixOf "hey" "oh, hey there!" `shouldBe` False
        it "can tell if a string ends with another one" $ do
            {- hint: use `isSuffixOf` -}
             L.isSuffixOf "there!" "oh, hey there!" `shouldBe` True
             L.isSuffixOf "there!" "oh, hey there" `shouldBe` False
        it "can partition lists based on a predicate" $ do
            {- hint: use `partition` at 3 -}
             L.partition (>4) [1,3,5,6,3,2,1,0,3,7]
                 `shouldBe` ([5,6,7],[1,3,3,2,1,0,3])
        it "finds the first match in a list" $ do
             L.find (>4) [1,2,3,4,5,6] `shouldBe` Just 5
             L.find (>7) [1,2,3,4,5,6] `shouldBe` Nothing
        it "finds the index of an element in a list" $ do
            {- hint: use `elemIndex` -}
             L.elemIndex 4 [1,2,3,4,5,6] `shouldBe` Just 3
             L.elemIndex 10 [1,2,3,4,5,6] `shouldBe` Nothing
        it "finds the index of a predicate" $ do
            {- hint: use `findIndex` -}
            L.findIndex(==4) [5,3,2,1,6,4] `shouldBe` Just 5
        it "can zip more than 2 lists" $ do
            {- hint: use `zipWith3` -}
             zipWith3 (\x y z -> x+y+z) [1,2,3] [4,5,2,2] [2,2,3]
                 `shouldBe` [7,9,8]
        it "creates a string array by line breaks" $ do
            {- hint: use `lines` -}
             lines "first line\nsecond line\nthird line"
                 `shouldBe` ["first line","second line", "third line"]
        it "creates a string from a string array" $ do
            {- hint: use `unlines` -}
             unlines ["first line","second line","third line"]
                 `shouldBe` "first line\nsecond line\nthird line\n"
        it "breaks a string apart into words" $ do
            {- hint: use `words` -}
             words "hey   these are the  words"
                 `shouldBe` ["hey","these","are","the","words"]
        it "can compact a list" $ do
            {- hint: use `nub` -}
             (L.nub [1,2,3,4,1,2,3,4,1,2,3,4]) `shouldBe` [1,2,3,4]
        it "combines two lists, removing duplicates" $ do
            {- hint: use `union` -}
             (L.union [1..7] [5..10]) `shouldBe` [1,2,3,4,5,6,7,8,9,10]
        it "finds the same items in two lists" $ do
            {- hint: use `intersect` -}
           L.intersect [1..7] [5..10] `shouldBe` [5,6,7]
        it "can sort a list of lists by their lengths" $ do
            {- hint: use `sortBy` and two functions with `on` -}
             let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]
             L.sortBy (on compare length) xs
                `shouldBe` [[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]
