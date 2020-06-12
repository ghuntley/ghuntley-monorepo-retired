import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO()
main = hspec $ do
    describe "Folds" $ do
        it "can sum up a list with foldl" $ do
            pending
            {- sum' [1,2,3,4] `shouldBe` 10 -}
        it "can check if an item is in a list" $ do
            pending
            {- elem' 3 [1,2,3,4] `shouldBe` True -}
            {- elem' 5 [1,2,3,4] `shouldBe` False -}
        it "can map over values with foldr" $ do
            pending
            {- map' (*2) [1,2,3] `shouldBe` [2,4,6] -}
        it "can find the maximum in a list" $ do
            pending
            {- maximum' [1,2,3] `shouldBe` 3 -}
        it "can reverse a list with foldl" $ do
            pending
            {- reverse' [1,3,5] `shouldBe` [5,3,1] -}
        it "can calculate a product of a list" $ do
            pending
            {- product' [2,2,3] `shouldBe` 12 -}
        it "can filter a list with foldr" $ do
            pending
            {- filter' (<3) [1,2,3,5] `shouldBe` [1,2] -}
        it "can pick the head of the list" $ do
            pending
            {- head' [5,4,3] `shouldBe` 5 -}
        it "can pick the last item of the list" $ do
            pending
            {- last' [5,4,3] `shouldBe` 3 -}
