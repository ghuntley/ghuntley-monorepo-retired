import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO()
main = hspec $ do
    describe "Scans are like folds" $ do
        it "they are reporting intermediate results" $ do
            pending
            [1,2,3] `shouldBe` [0,1,3,6]
            [1,2,3] `shouldBe` [6,5,3,0]
            {- keep only if they are larger than previous -}
            [3,4,5,3,7,9,2,1]
                `shouldBe` [3,4,5,5,7,9,9,9]
