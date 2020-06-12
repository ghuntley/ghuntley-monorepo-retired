import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.List as L
import Data.Function

{- search :: (Eq a) => [a] -> [a] -> Bool -}
{- search needle haystack = -}
    {- let nlen = length needle -}
    {- in foldl (\acc x -> if take nlen x == needle then True else acc) False (L.tails haystack) -}

main :: IO()
main = hspec $ do
    describe "More List functions" $ do
        it "can iterate as an infinite list" $ do
            pending
            {- (___ 10 $ ___ (*2) 1) `shouldBe` [1,2,4,8,16,32,64,128,256,512] -}
            {- (___ 3 $ ___ (++ "haha") "haha") -}
                {- `shouldBe` ["haha", "hahahaha", "hahahahahaha"] -}
        it "can split a list at the position specified" $ do
            pending
            {- ___ 3 "hey man" `shouldBe` ( "hey", " man") -}
            {- ___ ___ "hey man" `shouldBe` ("hey man", "") -}
            {- ___ 0 "hey man" `shouldBe` ("", "hey man") -}
        it "can exhaust a list with inits and tails" $ do
            pending
            {- hint: use `inits` and `tails` -}
            {- ___ "woot" `shouldBe` ["","w","wo","woo","woot"] -}
            {- ___ "woot" `shouldBe` ["woot", "oot", "ot", "t", ""] -}
        it "can tell if a string starts with another one" $ do
            pending
            {- hint: use `isPrefixOf` -}
            {- ___ "hey" "hey there!" `shouldBe` True -}
            {- ___ "hey" "oh, hey there!" `shouldBe` False -}
        it "can tell if a string ends with another one" $ do
            pending
            {- hint: use `isSuffixOf` -}
            {- ___ "there!" "oh, hey there!" `shouldBe` True -}
            {- ___ "there!" "oh, hey there" `shouldBe` False -}
        it "can partition lists based on a predicate" $ do
            pending
            {- hint: use `partition` at 3 -}
            {- ___ ___ [1,3,5,6,3,2,1,0,3,7] -}
                {- `shouldBe` ([5,6,7],[1,3,3,2,1,0,3]) -}
        it "finds the first match in a list" $ do
            pending
            {- ___ (>4) [1,2,3,4,5,6] `shouldBe` Just 5 -}
            {- ___ (>7) [1,2,3,4,5,6] `shouldBe` Nothing -}
        it "finds the index of an element in a list" $ do
            pending
            {- hint: use `elemIndex` -}
            {- ___ 4 [1,2,3,4,5,6] `shouldBe` Just 3 -}
            {- ___ 10 [1,2,3,4,5,6] `shouldBe` Nothing -}
        it "finds the index of a predicate" $ do
            pending
            {- hint: use `findIndex` -}
            {- ___ ___ (==4) [5,3,2,1,6,4] `shouldBe` Just 5 -}
        it "can zip more than 2 lists" $ do
            pending
            {- hint: use `zipWith3` -}
            {- ___ ___ [1,2,3] [4,5,2,2] [2,2,3] -}
                {- `shouldBe` [7,9,8] -}
        it "creates a string array by line breaks" $ do
            pending
            {- hint: use `lines` -}
            {- ___ "first line\nsecond line\nthird line" -}
                {- `shouldBe` ["first line","second line", "third line"] -}
        it "creates a string from a string array" $ do
            pending
            {- hint: use `unlines` -}
            {- ___ ["first line","second line","third line"] -}
                {- `shouldBe` "first line\nsecond line\nthird line\n" -}
        it "breaks a string apart into words" $ do
            pending
            {- hint: use `words` -}
            {- ___ "hey   these are the  words" -}
                {- `shouldBe` ["hey","these","are","the","words"] -}
        it "can compact a list" $ do
            pending
            {- hint: use `nub` -}
            {- ___ [1,2,3,4,1,2,3,4,1,2,3,4] `shouldBe` [1,2,3,4] -}
        it "combines two lists, removing duplicates" $ do
            pending
            {- hint: use `union` -}
            {- ___ [1..7] [5..10] `shouldBe` [1,2,3,4,5,6,7,8,9,10] -}
        it "finds the same items in two lists" $ do
            pending
            {- hint: use `intersect` -}
            {- ___ [1..7] [5..10] `shouldBe` [5,6,7] -}
        it "can sort a list of lists by their lengths" $ do
            pending
            {- hint: use `sortBy` and two functions with `on` -}
            {- let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]] -}
            {- ___ -}
                {- `shouldBe` [[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]] -}
