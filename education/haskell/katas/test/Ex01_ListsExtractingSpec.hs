module Ex01_ListsExtractingSpec
  ( spec
  ) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Extracting Portion of List" $ do
    it "finds the first element in a list" $ do
      head [1,2,3,4,5] `shouldBe` 1
    it "finds the tail part of a list" $ do
      pending
    it "finds the last element in a list" $ do
      pending
    it "extracts the elements except the last one from a list" $ do
      pending
    it "takes elements from a list" $ do
      pending
    it "can drop elements from a list" $ do
      pending
    it "can split a collection" $ do
      pending
    it "can take with a while" $ do
      pending
    it "can drop while" $ do
      pending
