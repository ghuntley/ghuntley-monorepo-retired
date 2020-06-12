import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO()
main = hspec $ do
    describe "Recursion" $ do
        it "calculates maximum" $ do
            pending
            {- maximum' [2,5,1] `shouldBe` 5 -}
        it "replicates items" $ do
            pending
            {- replicate' 5 'a' `shouldBe` "aaaaa" -}
        it "takes from a collection" $ do
            pending
            {- take' 3 "abcde" `shouldBe` "abc" -}
        it "reverses a collection" $ do
            pending
            {- reverse' [1,2,3] `shouldBe` [3,2,1] -}
        it "can repeat items" $ do
            pending
            {- take' 3 (repeat' 'a') `shouldBe` "aaa" -}
        it "can zip items" $ do
            pending
            {- zip' [1,2,3] ['a','b'] `shouldBe` [(1,'a'),(2,'b')] -}
        it "can check if an item is an element of a list" $ do
            pending
            {- myElem 3 [1,2,3] `shouldBe` True -}
        it "can do QuickSort - easily" $ do
            pending
            {- quicksort [3,1,2] `shouldBe` [1,2,3] -}
            {- quicksort "attila" `shouldBe` "aailtt" -}
