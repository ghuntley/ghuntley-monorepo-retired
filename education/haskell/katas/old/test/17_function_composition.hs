import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO()
main = hspec $ do
    describe "Function composition" $ do
        it "can take the absolute value and negate that" $ do
            pending
            {- use function composition-}
            {- take the abs value of items and negate them -}
            [1,2,-3,4,5]
                `shouldBe` [-1,-2,-3,-4,-5]
        it "can compose a new function from 3 other functions" $ do
            pending
            {- get the tail of the arrays, sum them and negate it -}
            {- [[1..5],[3..6],[1..7]] -}
                {- `shouldBe` [-14,-15,-27] -}
        it "can calculate the sum of all odd squares <10000" $ do
            pending
            {- 0 `shouldBe` 166650 -}
            {- A more readable version -}
            {- addSquareSum `shouldBe` 166650 -}

