module Ex03_ListsRangesSpec
  ( spec
  ) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "ranges" $ do
        it "can express ranges" $ do
            length [1..10] `shouldBe` 10
        it "can set steps" $ do
            take 3 [2,4..] `shouldBe` [2,4,6]
    describe "cycle" $ do
        it "can cycle through numbers" $ do
            take 5 (cycle [1,2,3]) `shouldBe` [1,2,3,1,2]
    describe "repeat" $ do
        it "can repeat numbers" $ do
            take 5 (repeat 3) `shouldBe` [3,3,3,3,3]
    describe "elem" $ do
        it "can remove non-uppercase letters" $ do
            let removeNonUppercase st = filter (flip elem ['A'..'Z']) st
            removeNonUppercase "IdontLIKEFROGS" `shouldBe` "ILIKEFROGS"
