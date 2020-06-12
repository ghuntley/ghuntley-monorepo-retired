import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.List
import qualified Data.Set as M {- use M.filter now -}

main :: IO()
main = hspec $ do
    describe "Dealing with Modules" $ do
        it "can pick up and use the nub function from Data.List" $ do
            pending
            {- find the number of unique (`nub`) elements in a list -}
            (length [1,1,2,3,4,4]) `shouldBe` 4
        it "inserts an item in between the list" $ do
            pending
            {- hint: use the `intersperse` function -}
            "MONKEY" `shouldBe` "M.O.N.K.E.Y"
            [1,2,3,4] `shouldBe` [1,0,2,0,3,0,4]
        it "inserts a list into a list and flattens it" $ do
            pending
            {- hint: use the `intercalate` function -}
            {- ["hey","there","guys"] -}
                {- `shouldBe` "hey there guys" -}
            {- [[1,2,3], [4,5,6]] -}
                {- `shouldBe` [1,2,3,0,0,4,5,6] -}
        it "transposes a matrix" $ do
            pending
            {- [[1,2,3],[4,5,6],[7,8,9]] -}
                {- `shouldBe` [[1,4,7],[2,5,8],[3,6,9]] -}
            {- ["hey","there","guys"] -}
                {- `shouldBe` ["htg","ehu","yey","rs","e"] -}
        it "concatenates lists" $ do
            pending
            {- ["foo","bar","car"] `shouldBe` "foobarcar" -}
            {- [[3,4,5],[6,7,8,9]] -}
                {- `shouldBe` [3,4,5,6,7,8,9] -}
        it "concatenates and maps a list" $ do
            pending
            {- ___ (replicate 3) [1..3] -}
                {- `shouldBe` [1,1,1,2,2,2,3,3,3] -}
        it "'and' can return True if all the elements are True" $ do
            pending
            {- (___ $ ___  (>4) [5,6,7,8]) `shouldBe` True -}
            {- (___ $ ___ (==4) [4,4,3,4,4]) `shouldBe` False -}
        it "'or' can return True if any elements are True" $ do
            pending
            {- (___ $ ___ (==4) [4,4,3,4,4]) `shouldBe` True -}
            {- (___ $ ___ (>4) [1..3]) `shouldBe` False -}
        it "'all' and 'any' works like it's expected" $ do
            pending
            {- (___ (==4) [2,3,5,6,1,4]) `shouldBe` True -}
            {- (___ (>4) [2,3,5,6,1,4]) `shouldBe` False -}
            {- (___ (`elem` ['A'..'Z']) "HEYGUYSwhatsup") -}
                {- `shouldBe` False -}
