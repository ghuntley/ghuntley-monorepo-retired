module Ex15_HofsScansSpec
  ( spec
  ) where

import Test.Hspec

main :: IO ()
main = hspec spec

sums' (x:xs) = scanl (\acc x -> acc+x) 0 (x:xs)

spec :: Spec
spec = do
    describe "Scans are like folds" $ do
        it "they are reporting intermediate results" $ do
            pending
            sums' [1,2,3] `shouldBe` [0,1,3,6]
            scanr (+) 0 [1,2,3] `shouldBe` [6,5,3,0]
            {- keep only if they are larger than previous -}
            scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
                `shouldBe` [3,4,5,5,7,9,9,9]
