import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "Extracting portion of list" $ do
        it "finds the first element in a list" $ do
            pending
            {- [1,2,3,4,5] `shouldBe` 1 -}
        it "finds the tail part of a list" $ do
            pending
            [1,2,3,4,5] `shouldBe` [2,3,4,5]
        it "finds the last element in a list" $ do
            pending
            {- [1,2,3,4,5] `shouldBe` 5 -}
        it "extracts the elements except the last one from a list" $ do
            pending
            [1,2,3,4,5] `shouldBe` [1,2,3,4]
        it "takes elements from a list" $ do
            pending
            ['a'..'z'] `shouldBe` "abcdefg"
            [1..5] `shouldBe` [1,2,3]
            (enumFromTo 10 100) `shouldBe` [10,11,12,13,14]
            take 3 (enumFrom 10) `shouldBe` [10,11,12,13,14]
        it "can drop elements from a list" $ do
            pending
            [1..10] `shouldBe` [6,7,8,9,10]
            ['a'..'g'] `shouldBe` "defg"
            (enumFromTo 10 20) `shouldBe` [16,17,18,19,20]
        it "can split a collection" $ do
            pending
            splitAt 2 [1..10] `shouldBe` ([1,2,3,4,5],[6,7,8,9,10])
        it "can take with a while" $ do
            pending
            [1..10] `shouldBe` [1,2]
            (enumFromTo 5 15) `shouldBe` [5,6,7]
            "abracadabra" `shouldBe` "a"
        it "can drop while" $ do
            pending
            [1..10] `shouldBe` [5,6,7,8,9,10]
            "abracadabra" `shouldBe` "bracadabra"
