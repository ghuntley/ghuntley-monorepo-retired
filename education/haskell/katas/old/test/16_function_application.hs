import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO()
main = hspec $ do
    describe "Function application with $" $ do
        it "let's get rid of parens" $ do
            pending
            {- double the list of 1-6 and sum its values -}
            41 `shouldBe` 42
            {- sqrt of 3 + 4 + 9 -}
            3 `shouldBe` 4
            {- double 1-5, sum of elements greater than 5 -}
            5 `shouldBe` 24
